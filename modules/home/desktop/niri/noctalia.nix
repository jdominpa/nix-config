{
  config,
  inputs,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.niri.noctalia;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    desktop.niri.noctalia.enable = lib.mkEnableOption "Whether to enable the configurations for Noctalia shell.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      imports = [
        inputs.noctalia.homeModules.default
      ];

      # Cursor settings
      home.pointerCursor = {
        gtk.enable = true;
        x11.enable = true;
        package = pkgs.bibata-cursors;
        name = "Bibata-Modern-Ice";
        size = 24;
      };
      gtk.enable = true;

      # Noctalia settings
      programs.noctalia-shell = {
        enable = true;
        settings = {
          appLauncher = {
            enableClipboardHistory = true;
            terminalCommand = "kitty -e";
            viewMode = "list";
          };
          audio = {
            preferredPlayer = "";
            volumeStep = 2;
          };
          bar = {
            backgroundOpacity = 0.33;
            density = "comfortable";
            floating = false;
            marginHorizontal = 0.25;
            marginVertical = 0.25;
            showCapsule = true;
            useSeparateOpacity = true;
            widgets = {
              center = [
                {
                  id = "Workspace";
                }
              ];
              left = [
                {
                  id = "ControlCenter";
                }
                {
                  id = "SystemMonitor";
                }
                {
                  id = "Launcher";
                }
                {
                  id = "ActiveWindow";
                }
              ];
              right = [
                {
                  id = "Tray";
                }
                {
                  id = "KeyboardLayout";
                  showIcon = false;
                }
                {
                  id = "MediaMini";
                }
                {
                  id = "Volume";
                }
                {
                  id = "NotificationHistory";
                }
                {
                  id = "Battery";
                }
                {
                  id = "Clock";
                  formatHorizontal = "HH:mm, dd.MM.yyyy";
                  tooltipFormat = "HH:mm, dd.MM.yyyy";
                }
              ];
            };
          };
          colorSchemes = {
            darkMode = true;
            predefinedScheme = "Catppuccin";
            useWallpaperColors = false;
          };
          controlCenter = {
            shortcuts = {
              left = [
                {
                  id = "WiFi";
                }
                {
                  id = "Network";
                }
                {
                  id = "Bluetooth";
                }
                {
                  id = "WallpaperSelector";
                }
              ];
              right = [
                {
                  id = "PowerProfile";
                }
                {
                  id = "NoctaliaPerformance";
                }
                {
                  id = "KeepAwake";
                }
                {
                  id = "NightLight";
                }
              ];
            };
            cards = [
              {
                enabled = true;
                id = "profile-card";
              }
              {
                enabled = true;
                id = "shortcuts-card";
              }
              {
                enabled = true;
                id = "audio-card";
              }
              {
                enabled = true;
                id = "brightness-card";
              }
              {
                enabled = true;
                id = "weather-card";
              }
              {
                enabled = true;
                id = "media-sysmon-card";
              }
            ];
          };
          dock.enabled = false;
          general = {
            animationSpeed = 1.5;
            shadowDirection = "bottom_right";
            shadowOffsetX = 2;
            shadowOffsetY = 3;
            telemetryEnabled = false;
          };
          location = {
            name = "Barcelona";
            firstDayOfWeek = 1;
          };
          nightLight.enabled = true;
          notifications = {
            enabled = true;
            sounds = {
              enabled = true;
              excludedApps = "discord,chrome,chromium";
            };
          };
          sessionMenu = {
            largeButtonsLayout = "grid";
            largeButtonsStyle = true;
          };
          ui = {
            fontDefault = "Aporetic Sans";
            fontFixed = "Aporetic Sans Mono";
            panelBackgroundOpacity = 0.85;
          };
          wallpaper = {
            directory = "";
            enabled = true;
            fillColor = "#000000";
            fillMode = "crop";
            monitorDirectories = [ ];
            overviewEnabled = false;
            randomEnabled = false;
            randomIntervalSec = 300;
            setWallpaperOnAllMonitors = true;
            solidColor = "#1a1a2e";
            transitionDuration = 1500;
            transitionEdgeSmoothness = 0.05;
            transitionType = "random";
            useSolidColor = false;
            wallpaperChangeMode = "random";
          };
        };
      };
    };
  };
}
