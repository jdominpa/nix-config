{
  config,
  lib,
  ...
}: let
  cfg = config.plasma-manager;
  virtualDesktopMax = builtins.length config.programs.plasma.kwin.virtualDesktops.names;
  shiftedNumbersMap = {
    "1" = "!";
    "2" = "@";
    "3" = "#";
    "4" = "$";
    "5" = "%";
  };
in {
  config = lib.mkIf cfg.enable {
    programs.plasma = {
      shortcuts = lib.mkMerge [
        {
          kwin =
            (builtins.listToAttrs (
              map (number: {
                name = "Switch to Desktop ${toString number}";
                value =
                  if number > 5 then
                    ""
                  else
                    "Meta+${toString number}";
              }) (lib.range 1 virtualDesktopMax)
            ))
            // (builtins.listToAttrs (
              map (number: {
                name = "Window to Desktop ${toString number}";
                value =
                  if shiftedNumbersMap ? "${toString number}" then
                    "Meta+${shiftedNumbersMap.${toString number}}"
                  else
                    "";
              }) (lib.range 1 virtualDesktopMax)
            ))
            // (builtins.listToAttrs (
              map (number: {
                name = "Switch to Screen ${toString (number - 1)}"; # Screens are 0th indexed
                value =
                  if number > 5 then
                    ""
                  else
                    "Meta+Alt+${toString number}";
              }) (lib.range 1 7)  # 7 is the maximum number of screens for KDE Plasma
            ))
            // (builtins.listToAttrs (
              map (number: {
                name = "Window to Screen ${toString (number - 1)}"; # Screens are 0th indexed
                value =
                  if number > 5 then
                    ""
                  else
                    "Meta+Ctrl+${toString number}";
              }) (lib.range 1 7)  # 7 is the maximum number of screens for KDE Plasma
            ));
        }
        {
          kwin = {
            "Window Close" = "Meta+Q";
            "Kill Window" = "Meta+Shift+Q";
            "Window Fullscreen" = "Meta+Shift+F";
            "Window Maximize" = "Meta+F";
            "Window Minimize" = "Meta+M";
            "MoveMouseToCenter" = "Meta+C";
            "MoveMouseToFocus" = "Meta+Shift+C";
            "Overview" = "Meta+W";
            "Show Desktop" = "Meta+D";
            "Switch Window Left" = "Meta+J";
            "Switch Window Down" = "Meta+K";
            "Switch Window Up" = "Meta+L";
            "Switch Window Right" = "Meta+;";
            "Window Quick Tile Left" = "Meta+Left";
            "Window Quick Tile Bottom" = "Meta+Down";
            "Window Quick Tile Top" = "Meta+Up";
            "Window Quick Tile Right" = "Meta+Right";
            "Toggle Window Raise/Lower" = "Meta+T";
            "Window On All Desktops" = "Meta+P";
            "Walk Through Windows" = "Meta+Tab";
            "Walk Through Windows (Reverse)" = "Meta+Shift+Tab";
          };
          "services/org.kde.krunner.desktop" = {
            _launch = builtins.concatStringsSep "\t" [
              "Meta+Space"
              "Search"
            ];
          };
          "services/org.kde.dolphin.desktop" = {
            _launch = "Meta+E";
          };
        }
        {
          "services/brave.desktop" = {
            new-window = "Meta+B";
          };
        }
        (lib.mkIf config.programs.konsole.enable {
          "services/org.kde.konsole.desktop" = {
            _launch = "Meta+X";
          };
        })
      ];
    };
  };
}
