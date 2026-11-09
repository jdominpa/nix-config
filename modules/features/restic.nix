{
  config,
  ...
}:
let
  inherit (config.user) username homeDirectory;
  storagebox = "u712081-sub1.your-storagebox.de";
  repository = "sftp://u712081-sub1@${storagebox}:23/restic";
  exclude = [ ".stversions" ];
  retryLock = "--retry-lock 30m";
  perHome = home: {
    passwordFile = "${home}/.config/restic/password";
    sshKey = "${home}/.ssh/storagebox";
    paths = map (dir: "${home}/${dir}") [
      "Documents"
      "Pictures"
    ];
  };
  linux = perHome homeDirectory.linux;
  darwin = perHome homeDirectory.darwin;
  sshSettings = {
    programs.ssh.knownHosts.storagebox = {
      hostNames = [ "[${storagebox}]:23" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIICf9svRenC/PLKIL9nk6K/pxQgoiFC41wTNvoIncOxs";
    };
  };
in
{
  flake.modules.nixos.restic = {
    imports = [ sshSettings ];
    services.restic.backups.storagebox = {
      initialize = true;
      user = username;
      inherit
        exclude
        repository
        ;
      inherit (linux) passwordFile;
      paths = linux.paths ++ [ "${homeDirectory.linux}/GrapheneOS/SeedVault" ];
      extraOptions = [ "sftp.args='-i ${linux.sshKey}'" ];
      checkOpts = [ retryLock ];
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
  };

  flake.modules.darwin.restic =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      home = homeDirectory.darwin;
      # `-o sftp.args` mirrors `extraOptions` in the nixos module.
      sftpOption = "-o sftp.args=${lib.escapeShellArg "-i ${darwin.sshKey}"}";
    in
    {
      imports = [ sshSettings ];
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
          pkgs.openssh # the sftp backend shells out to `ssh`
          pkgs.restic
        ];
        environment = {
          HOME = home; # restic keeps its cache under $HOME
          RESTIC_REPOSITORY = repository;
          RESTIC_PASSWORD_FILE = darwin.passwordFile;
        };
        # NOTE: The `--host` parameter assumes `config.networking.hostName` is
        # set.
        script = ''
          set -euo pipefail
          restic ${sftpOption} backup \
            --host ${lib.escapeShellArg config.networking.hostName} \
            ${retryLock} \
            ${lib.concatMapStringsSep " " (p: "--exclude ${lib.escapeShellArg p}") exclude} \
            ${lib.escapeShellArgs darwin.paths}
        '';
        serviceConfig = {
          StartCalendarInterval = [
            {
              Hour = 12;
              Minute = 0;
            }
          ];
          StandardOutPath = "${home}/Library/Logs/restic-backups-storagebox.log";
          StandardErrorPath = "${home}/Library/Logs/restic-backups-storagebox.log";
          LowPriorityIO = true;
          Nice = 10;
          ProcessType = "Background";
        };
      };
    };
}
