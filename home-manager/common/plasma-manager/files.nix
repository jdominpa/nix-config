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
          "KFileDialog Settings"."Places Icons Auto-resize" = false;
          "KFileDialog Settings"."Places Icons Static Size" = 22;
        };
        kded5rc = {
          "Module-browserintegrationreminder"."autoload" = false;
        };
        kwalletrc = {
          "Wallet"."Enabled" = false;
        };
        plasmarc = {
          "General"."RaiseMaximumVolume" = true;
          "General"."VolumeStep" = 2;
        };
      };
    };
  };
}
