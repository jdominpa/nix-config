{
  self,
  ...
}:
{
  flake.modules.nixos.noctalia-shell = {
    wrappers.noctalia-shell.enable = true;
  };

  flake.wrappers.noctalia-shell =
    {
      lib,
      pkgs,
      wlib,
      ...
    }:
    {
      imports = [ wlib.wrapperModules.noctalia-shell ];
      settings = {
        settingsVersion = 59;
        appLauncher = {
          terminalCommand = "${lib.getExe (self.wrappers.kitty.wrap { inherit pkgs; })} -e";
        };
        audio.volumeStep = 2;
        bar = {
          barType = "floating";
          showCapsule = false;
          widgetSpacing = 2;
          widgets = {
            left = [
              { id = "ControlCenter"; }
              { id = "Launcher"; }
              { id = "MediaMini"; }
            ];
            center = [
              { id = "Workspace"; }
            ];
            right = [
              { id = "Tray"; }
              {
                id = "NotificationHistory";
                hideWhenZero = true;
              }
              { id = "Network"; }
              { id = "Bluetooth"; }
              { id = "Volume"; }
              { id = "Battery"; }
              {
                id = "Clock";
                formatHorizontal = "HH:mm ddd, dd-MM-yyyy";
                tooltipFormat = "HH:mm ddd, dd-MM-yyyy";
              }
            ];
          };
        };
        brightness.brightnessStep = 2;
        colorSchemes = {
          darkMode = true;
          predefinedScheme = "Catppuccin";
        };
        dock.enabled = false;
        general = {
          animationSpeed = 1.2;
          clockFormat = "HH:mm\nddd dd-MM-yyyy";
        };
        idle = {
          enabled = true;
          screenOffTimeout = 600;
          lockTimeout = 660;
          suspendTimeout = 0;
        };
        location = {
          autoLocate = true;
          firstDayOfWeek = 1;
        };
        nightLight.autoSchedule = true;
        sessionMenu.powerOptions = [
          {
            action = "lock";
            enabled = true;
          }
          {
            action = "suspend";
            enabled = true;
          }
          {
            action = "logout";
            enabled = true;
          }
          {
            action = "reboot";
            enabled = true;
          }
          {
            action = "rebootToUefi";
            enabled = true;
          }
          {
            action = "shutdown";
            enabled = true;
          }
        ];
        ui.panelBackgroundOpacity = 1;
        wallpaper.directory = "~/Imatges/Wallpapers";
      };
    };
}
