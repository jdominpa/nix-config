{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.desktop.plasma-manager;
  user = config.jdp.base.user;
in
{
  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.plasma = {
        configFile = {
          dolphinrc = {
            "KFileDialog Settings" = {
              "Places Icons Auto-resize" = false;
              "Places Icons Static Size" = 22;
            };
            CompactMode.IconSize = 22;
            DetailsMode = {
              IconSize = 22;
              PreviewSize = 22;
            };
          };
          kded5rc = {
            Module-browserintegrationreminder.autoload = false;
          };
          ksmserverrc = {
            General.loginMode = "emptySession";
          };
          kwalletrc = {
            Wallet.Enabled = false;
          };
          plasmarc = {
            General = {
              RaiseMaximumVolume = true;
              VolumeStep = 2;
            };
          };
        };
      };
    };
  };
}
