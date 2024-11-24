{
  config,
  lib,
  ...
}: let
  cfg = config.jdp.home.desktop.plasma-manager;
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
        tiling = {
          padding = 15;
          layout = {
            id = "82dece0e-5d1e-527e-a891-21c2c84c6f64";
            tiles = {
              layoutDirection = "horizontal";
              tiles = [
                {
                  width = 0.5;
                }
                {
                  layoutDirection = "vertical";
                  tiles = [
                    {
                      height = 0.5;
                    }
                    {
                      height = 0.5;
                    }
                  ];
                  width = 0.5;
                }
              ];
            };
          };
        };
      };
    };
  };
}
