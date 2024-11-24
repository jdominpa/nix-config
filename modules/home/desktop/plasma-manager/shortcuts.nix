{
  config,
  lib,
  ...
}: let
  cfg = config.jdp.home.desktop.plasma-manager;
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
            # Switch to desktop
            (builtins.listToAttrs (
              map (number: {
                name = "Switch to Desktop ${toString number}";
                value =
                  if number > 5
                  then ""
                  else "Meta+${toString number}";
              }) (lib.range 1 virtualDesktopMax)
            ))
            # Move window to desktop
            // (builtins.listToAttrs (
              map (number: {
                name = "Window to Desktop ${toString number}";
                value =
                  if shiftedNumbersMap ? "${toString number}"
                  then "Meta+${shiftedNumbersMap.${toString number}}"
                  else "";
              }) (lib.range 1 virtualDesktopMax)
            ))
            # Switch to screen
            // (builtins.listToAttrs (
              map (number: {
                name = "Switch to Screen ${toString (number - 1)}"; # Screens are 0th indexed
                value =
                  if number > 5
                  then ""
                  else "Meta+Alt+${toString number}";
              }) (lib.range 1 7) # 7 is the maximum number of screens for KDE Plasma
            ))
            # Move window to screen
            // (builtins.listToAttrs (
              map (number: {
                name = "Window to Screen ${toString (number - 1)}"; # Screens are 0th indexed
                value =
                  if number > 5
                  then ""
                  else "Meta+Ctrl+${toString number}";
              }) (lib.range 1 7) # 7 is the maximum number of screens for KDE Plasma
            ));
        }
        {
          kwin = {
            # General shortcuts
            "Window Close" = "Meta+Q";
            "Kill Window" = "Meta+Shift+Q";
            "Window Fullscreen" = "Meta+Shift+F";
            "Window Maximize" = "Meta+F";
            "Window Minimize" = "Meta+M";
            "Overview" = "Meta+W";
            "Show Desktop" = "Meta+D";
            "Window On All Desktops" = "Meta+P";
            # Window movement
            "Switch Window Left" = "Meta+J";
            "Switch Window Down" = "Meta+K";
            "Switch Window Up" = "Meta+L";
            "Switch Window Right" = "Meta+;";
            # Window tiling
            "Window Quick Tile Left" = "Meta+Shift+J";
            "Window Quick Tile Bottom" = "Meta+Shift+K";
            "Window Quick Tile Top" = "Meta+Shift+L";
            "Window Quick Tile Right" = "Meta+:"; # equivalent to Meta+Shift+;
            # Window swapping
            "Walk Through Windows" = "Alt+Tab";
            "Walk Through Windows (Reverse)" = "Alt+Shift+Tab";
            # Screen movement
            "Switch to Previous Screen" = "Meta+I";
            "Switch to Next Screen" = "Meta+O";
            "Window to Previous Screen" = "Meta+Shift+I";
            "Window to Next Screen" = "Meta+Shift+O";
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
        (lib.mkIf config.programs.konsole.enable {
          "services/org.kde.konsole.desktop" = {
            _launch = "Meta+X";
          };
        })
      ];
    };
  };
}
