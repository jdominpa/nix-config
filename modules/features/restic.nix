{
  config,
  ...
}:
let
  inherit (config.user) homeDirectory;
in
{
  flake.modules.nixos.restic = {
    services.restic.backups.storagebox = {
      initialize = true;
      repository = "sftp://u712081-sub1@u712081-sub1.your-storagebox.de:23/restic";
      passwordFile = "/var/lib/restic/password";
      paths = [
        "${homeDirectory.linux}/Documents"
        "${homeDirectory.linux}/Pictures"
        "${homeDirectory.linux}/GrapheneOS/SeedVault"
      ];
      exclude = [
        ".stversions"
      ];
      extraOptions = [
        "sftp.args='-i /root/.ssh/storagebox'"
      ];
      timerConfig = {
        OnCalendar = "daily";
        Persistent = true; # catch up if the machine was off at the scheduled time
      };
      pruneOpts = [
        "--keep-daily 7"
        "--keep-weekly 5"
        "--keep-monthly 12"
      ];
    };
  };
}
