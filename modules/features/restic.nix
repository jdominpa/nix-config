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
  perHome = home: {
    passwordFile = "${home}/.config/restic/password";
    # Optional: holds a healthchecks.io ping URL (see `mkPing`)
    healthcheckFile = "${home}/.config/restic/healthchecks-url";
    sshKey = "${home}/.ssh/storagebox";
    paths = map (dir: "${home}/${dir}") [
      "Documents"
      "Pictures"
      "GrapheneOS"
    ];
  };
  sshSettings = {
    programs.ssh.knownHosts.storagebox = {
      hostNames = [
        "[${storagebox}]:23"
        "[${resticStoragebox}]:23"
      ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIICf9svRenC/PLKIL9nk6K/pxQgoiFC41wTNvoIncOxs";
    };
  };
  # Monitors and reports the outcome of a run: the bare URL on success or
  # `<url>/fail` on failure. A failure that cannot be reported (no network)
  # still surfaces, because the monitor alerts when the success ping stops
  # arriving.
  mkPing =
    pkgs: healthcheckFile:
    pkgs.writeShellScript "restic-storagebox-ping" ''
      # usage: restic-storagebox-ping success|fail
      url_file=${lib.escapeShellArg healthcheckFile}
      # Monitoring is optional: without the file there is nothing to report to.
      [ -r "$url_file" ] || exit 0
      url=$(< "$url_file")
      if [ "''${1:-fail}" != success ]; then
        url="$url/fail"
      fi
      exec ${lib.getExe pkgs.curl} --fail --silent --show-error \
        --max-time 10 --retry 5 --output /dev/null "$url"
    '';
in
{
  flake.modules.nixos.restic =
    { pkgs, ... }:
    let
      linux = perHome homeDirectory.linux;
      ping = mkPing pkgs linux.healthcheckFile;
    in
    {
      imports = [ sshSettings ];
      services.restic.backups.storagebox = {
        initialize = true;
        user = username;
        inherit exclude repository;
        inherit (linux) passwordFile paths;
        extraOptions = [ "sftp.args='-i ${linux.sshKey}'" ];
        checkOpts = [
          retryLock
          # Besides the structural check, re-read a random sample of the pack
          # files on every run, to catch bit rot.
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
      systemd.services = {
        # The unit runs backup, `forget --prune` and `check` in turn, so
        # success is reported only when all three succeeded.
        restic-backups-storagebox = {
          onSuccess = [ "restic-storagebox-ping@success.service" ];
          onFailure = [ "restic-storagebox-ping@fail.service" ];
        };
        "restic-storagebox-ping@" = {
          description = "Report the restic backup result (%i) to the monitor";
          wants = [ "network-online.target" ];
          after = [ "network-online.target" ];
          serviceConfig = {
            Type = "oneshot";
            User = username;
            ExecStart = "${ping} %i";
          };
        };
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
      darwin = perHome homeDirectory.darwin;
      home = homeDirectory.darwin;
      logFile = "${home}/Library/Logs/restic-backups-storagebox.log";
      ping = mkPing pkgs darwin.healthcheckFile;
      # `-o sftp.args` mirrors `extraOptions` in the nixos module.
      sftpOption = "-o sftp.args=${lib.escapeShellArg "-i ${darwin.sshKey}"}";
    in
    {
      imports = [ sshSettings ];
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
          export RESTIC_PASSWORD_FILE=${lib.escapeShellArg darwin.passwordFile}
          exec ${lib.getExe pkgs.restic} ${sftpOption} "$@"
        '')
      ];
      # Stands in for the systemd timer and its two units: there is no
      # `services.restic` on darwin. Pruning stays on the nixos host.
      launchd.user.agents.restic-backups-storagebox = {
        path = [
          pkgs.coreutils # `date`, `tail`, `wc`, `cat`, `rm` in the script below
          pkgs.openssh # the sftp backend shells out to `ssh`
          pkgs.restic
        ];
        environment = {
          HOME = home; # restic keeps its cache under $HOME
          RESTIC_REPOSITORY = repository;
          RESTIC_PASSWORD_FILE = darwin.passwordFile;
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
          status=0
          restic ${sftpOption} backup \
            --host ${lib.escapeShellArg config.networking.hostName} \
            ${retryLock} \
            ${lib.concatMapStringsSep " " (p: "--exclude ${lib.escapeShellArg p}") exclude} \
            ${lib.escapeShellArgs darwin.paths} || status=$?
          echo "=== $(date '+%F %T') backup finished, exit status $status ==="

          # Only exit status 0 counts as success. Status 3 means a snapshot was
          # written but some files could not be read, which is how a TCC denial
          # would show up, so it is reported as a failure on purpose.
          if [ "$status" -eq 0 ]; then
            ${ping} success || true
          else
            ${ping} fail || true
          fi
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
