{
  config,
  lib,
  ...
}:
let
  inherit (config.user) username homeDirectory;
  box = "u712081";
  storagebox = "${box}.your-storagebox.de";
  subAccount = "${box}-sub1";
  resticStoragebox = "${subAccount}.your-storagebox.de";
  repository = "sftp://${subAccount}@${resticStoragebox}:23/restic";
  exclude = [ ".stversions" ];
  retryLock = "--retry-lock 30m";
  # Kill a run that is still going after this long, so that a stalled run is
  # reported as a failure and the next scheduled run can start.
  runTimeout = "6h";
  # Keepalives make ssh drop a dead connection after about five minutes,
  # instead of leaving restic waiting on it forever.
  sftpArgsForKey = sshKey: "-i ${sshKey} -o ServerAliveInterval=60 -o ServerAliveCountMax=5";
  pathsForHome =
    home:
    map (dir: "${home}/${dir}") [
      "Documents"
      "Pictures"
      "GrapheneOS"
    ];
  secretsNames = {
    passwordFile = "restic/password";
    healthcheckFile = "restic/healthcheck-url";
    sshKey = "restic/storagebox-ssh-key";
  };
  secretsSettings =
    { config, ... }:
    {
      sops.secrets = lib.genAttrs (lib.attrValues secretsNames) (_: config.secretPresets.userReadable);
    };
  secretsForConfig = config: lib.mapAttrs (_: name: config.sops.secrets.${name}.path) secretsNames;
  sshSettings = {
    programs.ssh.knownHosts.storagebox = {
      hostNames = [
        "[${storagebox}]:23"
        "[${resticStoragebox}]:23"
      ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIICf9svRenC/PLKIL9nk6K/pxQgoiFC41wTNvoIncOxs";
    };
  };
  # Reports a run to the monitor: `start` when it begins, then its exit status
  # (0 is success, anything else a failure; a non-numeric status, such as the
  # signal name of a killed run, is sent as `fail`). A failure that cannot be
  # reported (no network) still surfaces, because the monitor alerts when the
  # success ping stops arriving, or when a started run does not finish.
  mkPing =
    pkgs: healthcheckFile:
    pkgs.writeShellScript "restic-storagebox-ping" ''
      # usage: restic-storagebox-ping start|<exit status>
      set -eu
      url=$(< ${lib.escapeShellArg healthcheckFile})
      case "''${1:-}" in
        start) url="$url/start" ;;
        "" | *[!0-9]*) url="$url/fail" ;;
        *) url="$url/$1" ;;
      esac
      exec ${lib.getExe pkgs.curl} --fail --silent --show-error \
        --max-time 10 --retry 3 --output /dev/null "$url"
    '';
in
{
  flake.modules.nixos.restic =
    { config, pkgs, ... }:
    let
      secrets = secretsForConfig config;
      ping = mkPing pkgs secrets.healthcheckFile;
    in
    {
      imports = [
        secretsSettings
        sshSettings
      ];
      services.restic.backups.storagebox = {
        user = username;
        inherit exclude repository;
        inherit (secrets) passwordFile;
        paths = pathsForHome homeDirectory.linux;
        extraOptions = [ "sftp.args='${sftpArgsForKey secrets.sshKey}'" ];
        extraBackupArgs = [ retryLock ];
        checkOpts = [
          retryLock
          # Re-read a random sample of the pack files on every run, to catch bit
          # rot.
          "--read-data-subset 500M"
        ];
        timerConfig = {
          OnCalendar = "daily";
          Persistent = true; # catch up if the machine was off at the scheduled time
        };
        pruneOpts = [
          "--keep-daily 7"
          "--keep-weekly 5"
          "--keep-monthly 12"
          "--group-by host"
          retryLock
        ];
      };
      systemd.services.restic-backups-storagebox = {
        preStart = lib.mkBefore "${ping} start || true";
        postStop = lib.mkAfter ''${ping} "$EXIT_STATUS" || true'';
        serviceConfig.TimeoutStartSec = runTimeout;
      };
    };

  flake.modules.darwin.restic =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      secrets = secretsForConfig config;
      home = homeDirectory.darwin;
      logFile = "${home}/Library/Logs/restic-backups-storagebox.log";
      ping = mkPing pkgs secrets.healthcheckFile;
      # `-o sftp.args` mirrors `extraOptions` in the nixos module.
      sftpOption = "-o sftp.args=${lib.escapeShellArg (sftpArgsForKey secrets.sshKey)}";
    in
    {
      imports = [
        secretsSettings
        sshSettings
      ];
      assertions = [
        {
          # `lib.escapeShellArg null` is `''`, which would silently become
          # `--host ''` in the script below.
          assertion = config.networking.hostName != null;
          message = "restic: networking.hostName must be set; it is the `--host` this machine's snapshots are recorded against.";
        }
      ];
      environment.systemPackages = [
        # Hand-rolled equivalent of the wrapper the nixos module generates with
        # `createWrapper`.
        (pkgs.writeShellScriptBin "restic-storagebox" ''
          export RESTIC_REPOSITORY=${lib.escapeShellArg repository}
          export RESTIC_PASSWORD_FILE=${lib.escapeShellArg secrets.passwordFile}
          exec ${lib.getExe pkgs.restic} ${sftpOption} "$@"
        '')
      ];
      # Stands in for the systemd timer and its two units: there is no
      # `services.restic` on darwin. Pruning stays on the nixos host.
      launchd.user.agents.restic-backups-storagebox = {
        path = [
          pkgs.coreutils # `date`, `tail`, `wc`, `cat`, `rm`, `seq`, `sleep`, `timeout` below
          pkgs.openssh # the sftp backend shells out to `ssh`
          pkgs.restic
        ];
        environment = {
          HOME = home; # restic keeps its cache under $HOME
          RESTIC_REPOSITORY = repository;
          RESTIC_PASSWORD_FILE = secrets.passwordFile;
        };
        script = ''
          set -uo pipefail

          # launchd appends to the log forever. Once it passes 1 MiB, keep only
          # its last 2000 lines. Rewriting it in place is safe because launchd
          # holds the file open in append mode, so this run's output still
          # lands at the new end of the file.
          log=${lib.escapeShellArg logFile}
          if [ -f "$log" ] && [ "$(wc -c < "$log")" -gt 1048576 ]; then
            tail -n 2000 "$log" > "$log.tmp"
            cat "$log.tmp" > "$log"
            rm -f "$log.tmp"
          fi

          echo "=== $(date '+%F %T') backup started ==="
          # After a wake, launchd starts a missed run before the network is
          # back. Wait up to 10 minutes for the Storage Box to answer; if it
          # never does, run anyway, so that the failure is logged and reported.
          for _ in $(seq 20); do
            /usr/bin/nc -z -G 5 ${resticStoragebox} 23 2> /dev/null && break
            sleep 30
          done
          ${ping} start || true

          status=0
          # `timeout` exits with 124 when it kills a stalled run.
          timeout --kill-after=5m ${runTimeout} \
            restic ${sftpOption} backup \
            --host ${lib.escapeShellArg config.networking.hostName} \
            ${retryLock} \
            ${lib.concatMapStringsSep " " (p: "--exclude ${lib.escapeShellArg p}") exclude} \
            ${lib.escapeShellArgs (pathsForHome home)} || status=$?
          echo "=== $(date '+%F %T') backup finished, exit status $status ==="

          # Only exit status 0 counts as success. Status 3 means a snapshot was
          # written but some files could not be read, which is how a TCC denial
          # would show up, so it is reported as a failure on purpose.
          ${ping} "$status" || true
          exit "$status"
        '';
        serviceConfig = {
          StartCalendarInterval = [
            {
              Hour = 12;
              Minute = 0;
            }
          ];
          StandardOutPath = logFile;
          StandardErrorPath = logFile;
          LowPriorityIO = true;
          Nice = 10;
          ProcessType = "Background";
        };
      };
    };
}
