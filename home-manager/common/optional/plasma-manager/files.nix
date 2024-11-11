{
  config,
  lib,
  ...
}: let
  cfg = config.plasma-manager;
in {
  config = lib.mkIf cfg.enable {
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
}
