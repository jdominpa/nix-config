{
  config,
  lib,
  ...
}: let
  cfg = config.plasma-manager;
in {
  config = lib.mkIf cfg.enable {
    programs.plasma = {
      windows.allowWindowsToRememberPositions = true;
      kwin = {
        effects = {
          shakeCursor.enable = false;
        };
        nightLight = {
          enable = true;
          mode = "location";
          location = {
            latitude = "40.463669";
            longitude = "-3.749220";
          };
          temperature = {
            day = 6500;
            night = 3500;
          };
        };
        virtualDesktops = let
          number = 5;
        in {
          names = map (n: "Desktop ${toString n}") (lib.range 1 number);
          inherit number;
          rows = 1;
        };
        tiling.padding = 10;
        titlebarButtons = {
          left = [
            "on-all-desktops"
          ];
          right = [
            "minimize"
            "maximize"
            "close"
          ];
        };
      };
    };
  };
}
