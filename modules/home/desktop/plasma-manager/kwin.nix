{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.plasma-manager;
  inherit (config.jdp.base) user;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
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
          virtualDesktops =
            let
              number = 5;
            in
            {
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
        };
      };
    };
  };
}
