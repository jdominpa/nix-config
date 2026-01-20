{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.home.apps.ghostty;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    apps.ghostty.enable = lib.mkEnableOption "Install Ghostty terminal.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.ghostty = {
        enable = true;
        themes = {
          modus-operandi = {
            background = "FFFFFF";
            cursor-color = "000000";
            foreground = "000000";
            palette = [
              "0=#000000"
              "1=#a60000"
              "2=#005e00"
              "3=#813e00"
              "4=#0031a9"
              "5=#721045"
              "6=#00538b"
              "7=#bfbfbf"
              "8=#595959"
              "9=#972500"
              "10=#315b00"
              "11=#70480f"
              "12=#2544bb"
              "13=#5317ac"
              "14=#005a5f"
              "15=#ffffff"
            ];
            selection-background = "bdbdbd";
            selection-foreground = "000000";
          };
          modus-vivendi = {
            background = "000000";
            cursor-color = "FFFFFF";
            foreground = "FFFFFF";
            palette = [
              "0=#000000"
              "1=#ff8059"
              "2=#44bc44"
              "3=#d0bc00"
              "4=#2fafff"
              "5=#feacd0"
              "6=#00d3d0"
              "7=#bfbfbf"
              "8=#595959"
              "9=#ef8b50"
              "10=#70b900"
              "11=#c0c530"
              "12=#79a8ff"
              "13=#b6a0ff"
              "14=#6ae4b9"
              "15=#ffffff"
            ];
            selection-background = "5a5a5a";
            selection-foreground = "FFFFFF";
          };
        };
        settings = {
          cursor-style = "block";
          cursor-style-blink = false;
          font-family = "Aporetic Sans Mono";
          font-size = 13;
          theme = "light:modus-operandi,dark:modus-vivendi";
        };
      };
    };
  };
}
