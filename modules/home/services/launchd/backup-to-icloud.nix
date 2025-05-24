{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  cfg = config.jdp.home.services.launchd.backupToiCloud;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    services.launchd.backupToiCloud.enable = lib.mkEnableOption "Whether to enable a launchd service that copies the contents of ~/Documents to iCloud/Documents";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = isDarwin;
        message = "Must use Darwin system for modules that require Launchd: backup-documents-to-icloud";
      }
    ];

    # NOTE: for backup-documents-to-icloud to work rsync must have full disk
    # access
    home-manager.users.${user.name} = {
      launchd.agents = {
        "backup-documents-to-icloud" = {
          enable = true;
          config = {
            Program = /usr/bin/rsync;
            ProgramArguments = [
              "/usr/bin/rsync"
              "-vhaP"
              "--exclude=.stfolder"
              "--exclude=.stignore"
              "--delete-during"
              "--delete-excluded"
              "${user.homeDirectory}/Documents/"
              "${user.homeDirectory}/Library/Mobile Documents/com~apple~CloudDocs/Documents"
            ];
            StandardErrorPath = "${user.homeDirectory}/.local/backup-documents-to-icloud/stderr.txt";
            StandardOutPath = "${user.homeDirectory}/.local/backup-documents-to-icloud/stdout.txt";
            StartCalendarInterval = [
              {
                Hour = 10;
                Minute = 0;
              }
            ];
          };
        };
      };
    };
  };
}
