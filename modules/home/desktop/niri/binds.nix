{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.niri;
  inherit (config.jdp.base) user;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.niri.settings.binds = {
        "Mod+Shift+Slash".action.show-hotkey-overlay = [ ];
        "Mod+T" = {
          action.spawn = "kitty";
          hotkey-overlay = {
            title = "Open a terminal";
          };
        };
        "Mod+Space" = {
          action.spawn = [
            "noctalia-shell"
            "ipc"
            "call"
            "launcher"
            "toggle"
          ];
          hotkey-overlay = {
            title = "Run application launcher";
          };
        };
        "Mod+P".action.screenshot = [ ];
        "Mod+Ctrl+P".action.screenshot-window = [ ];
        "Mod+Alt+P".action.screenshot-screen = [ ];
        "Mod+Escape".action.toggle-keyboard-shortcuts-inhibit = [ ];
        "Mod+Alt+L" = {
          action.spawn = [
            "noctalia-shell"
            "ipc"
            "call"
            "lockScreen"
            "lock"
          ];
          hotkey-overlay = {
            title = "Lock the screen";
          };
        };
        "Mod+Shift+Z".action.power-off-monitors = [ ];
        "Mod+Shift+Q".action.quit = [ ];
        "Ctrl+Alt+Delete".action.quit = [ ];

        # Media and brightness bindings
        "XF86AudioRaiseVolume" = {
          action.spawn = [
            "wpctl"
            "set-volume"
            "@DEFAULT_AUDIO_SINK@"
            "0.01+"
          ];
          allow-when-locked = true;
        };
        "XF86AudioLowerVolume" = {
          action.spawn = [
            "wpctl"
            "set-volume"
            "@DEFAULT_AUDIO_SINK@"
            "0.01-"
          ];
          allow-when-locked = true;
        };
        "XF86AudioMute" = {
          action.spawn = [
            "wpctl"
            "set-mute"
            "@DEFAULT_AUDIO_SINK@"
            "toggle"
          ];
          allow-when-locked = true;
          repeat = false;
        };
        "XF86AudioMicMute" = {
          action.spawn = [
            "wpctl"
            "set-mute"
            "@DEFAULT_AUDIO_SOURCE@"
            "toggle"
          ];
          allow-when-locked = true;
          repeat = false;
        };
        "XF86AudioPlay" = {
          action.spawn = [
            "playerctl"
            "play-pause"
          ];
          allow-when-locked = true;
          repeat = false;
        };
        "XF86AudioStop" = {
          action.spawn = [
            "playerctl"
            "stop"
          ];
          allow-when-locked = true;
          repeat = false;
        };
        "XF86AudioPrev" = {
          action.spawn = [
            "playerctl"
            "previous"
          ];
          allow-when-locked = true;
          repeat = false;
        };
        "XF86AudioNext" = {
          action.spawn = [
            "playerctl"
            "next"
          ];
          allow-when-locked = true;
          repeat = false;
        };
        "XF86MonBrightnessUp" = {
          action.spawn = [
            "brightnessctl"
            "--class=backlight"
            "set"
            "+10%"
          ];
          allow-when-locked = true;
        };
        "XF86MonBrightnessDown" = {
          action.spawn = [
            "brightnessctl"
            "--class=backlight"
            "set"
            "10%-"
          ];
          allow-when-locked = true;
        };

        # Navigation bindings
        "Mod+F" = {
          action.toggle-overview = [ ];
          repeat = false;
        };
        "Mod+Left".action.focus-column-left = [ ];
        "Mod+Down".action.focus-window-down = [ ];
        "Mod+Up".action.focus-window-up = [ ];
        "Mod+Right".action.focus-column-right = [ ];
        "Mod+J".action.focus-column-left = [ ];
        "Mod+K".action.focus-window-down = [ ];
        "Mod+L".action.focus-window-up = [ ];
        "Mod+Semicolon".action.focus-column-right = [ ];
        "Mod+WheelScrollLeft".action.focus-column-left = [ ];
        "Mod+WheelScrollRight".action.focus-column-right = [ ];

        "Mod+Home".action.focus-column-first = [ ];
        "Mod+End".action.focus-column-last = [ ];

        "Mod+Ctrl+Left".action.focus-monitor-left = [ ];
        "Mod+Ctrl+Down".action.focus-monitor-down = [ ];
        "Mod+Ctrl+Up".action.focus-monitor-up = [ ];
        "Mod+Ctrl+Right".action.focus-monitor-right = [ ];
        "Mod+Ctrl+J".action.focus-monitor-left = [ ];
        "Mod+Ctrl+K".action.focus-monitor-down = [ ];
        "Mod+Ctrl+L".action.focus-monitor-up = [ ];
        "Mod+Ctrl+Semicolon".action.focus-monitor-right = [ ];

        "Mod+I".action.focus-workspace-down = [ ];
        "Mod+O".action.focus-workspace-up = [ ];
        "Mod+WheelScrollDown" = {
          action.focus-workspace-down = [ ];
          cooldown-ms = 150;
        };
        "Mod+WheelScrollUp" = {
          action.focus-workspace-up = [ ];
          cooldown-ms = 150;
        };

        "Mod+1".action.focus-workspace = 1;
        "Mod+2".action.focus-workspace = 2;
        "Mod+3".action.focus-workspace = 3;
        "Mod+4".action.focus-workspace = 4;
        "Mod+5".action.focus-workspace = 5;
        "Mod+6".action.focus-workspace = 6;
        "Mod+7".action.focus-workspace = 7;
        "Mod+8".action.focus-workspace = 8;
        "Mod+9".action.focus-workspace = 9;

        # Window moving bindings
        "Mod+Q" = {
          action.close-window = [ ];
          repeat = false;
        };
        "Mod+Shift+Left".action.move-column-left = [ ];
        "Mod+Shift+Down".action.move-window-down = [ ];
        "Mod+Shift+Up".action.move-window-up = [ ];
        "Mod+Shift+Right".action.move-column-right = [ ];
        "Mod+Shift+J".action.move-column-left = [ ];
        "Mod+Shift+K".action.move-window-down = [ ];
        "Mod+Shift+L".action.move-window-up = [ ];
        "Mod+Shift+Semicolon".action.move-column-right = [ ];
        "Mod+Shift+WheelScrollLeft".action.move-column-left = [ ];
        "Mod+Shift+WheelScrollRight".action.move-column-right = [ ];

        "Mod+Shift+Home".action.move-column-to-first = [ ];
        "Mod+Shift+End".action.move-column-to-last = [ ];

        "Mod+Ctrl+Shift+Left".action.move-column-to-monitor-left = [ ];
        "Mod+Ctrl+Shift+Down".action.move-column-to-monitor-down = [ ];
        "Mod+Ctrl+Shift+Up".action.move-column-to-monitor-up = [ ];
        "Mod+Ctrl+Shift+Right".action.move-column-to-monitor-right = [ ];
        "Mod+Ctrl+Shift+J".action.move-column-to-monitor-left = [ ];
        "Mod+Ctrl+Shift+K".action.move-column-to-monitor-down = [ ];
        "Mod+Ctrl+Shift+L".action.move-column-to-monitor-up = [ ];
        "Mod+Ctrl+Shift+Semicolon".action.move-column-to-monitor-right = [ ];

        "Mod+Shift+I".action.move-column-to-workspace-down = [ ];
        "Mod+Shift+O".action.move-column-to-workspace-up = [ ];
        "Mod+Ctrl+Shift+I".action.move-workspace-down = [ ];
        "Mod+Ctrl+Shift+O".action.move-workspace-up = [ ];
        "Mod+Shift+WheelScrollDown" = {
          action.move-workspace-down = [ ];
          cooldown-ms = 150;
        };
        "Mod+Shift+WheelScrollUp" = {
          action.move-workspace-up = [ ];
          cooldown-ms = 150;
        };

        "Mod+Shift+1".action.move-column-to-workspace = 1;
        "Mod+Shift+2".action.move-column-to-workspace = 2;
        "Mod+Shift+3".action.move-column-to-workspace = 3;
        "Mod+Shift+4".action.move-column-to-workspace = 4;
        "Mod+Shift+5".action.move-column-to-workspace = 5;
        "Mod+Shift+6".action.move-column-to-workspace = 6;
        "Mod+Shift+7".action.move-column-to-workspace = 7;
        "Mod+Shift+8".action.move-column-to-workspace = 8;
        "Mod+Shift+9".action.move-column-to-workspace = 9;

        "Mod+Comma".action.consume-window-into-column = [ ];
        "Mod+Period".action.expel-window-from-column = [ ];

        # Window management bindings
        "Mod+R".action.switch-preset-column-width = [ ];
        "Mod+Shift+R".action.switch-preset-window-height = [ ];
        "Mod+Ctrl+R".action.reset-window-height = [ ];
        "Mod+M".action.maximize-column = [ ];
        "Mod+Shift+M".action.fullscreen-window = [ ];
        "Mod+Ctrl+M".action.expand-column-to-available-width = [ ];
        "Mod+Alt+M".action.maximize-window-to-edges = [ ];
        "Mod+C".action.center-column = [ ];
        "Mod+Minus".action.set-column-width = "-10%";
        "Mod+Equal".action.set-column-width = "+10%";
        "Mod+Ctrl+Minus".action.set-column-width = "-1";
        "Mod+Ctrl+Equal".action.set-column-width = "+1";
        "Mod+Shift+Minus".action.set-window-height = "-10%";
        "Mod+Shift+Equal".action.set-window-height = "+10%";
        "Mod+Ctrl+Shift+Minus".action.set-window-height = "-1";
        "Mod+Ctrl+Shift+Equal".action.set-window-height = "+1";
        "Mod+V".action.toggle-window-floating = [ ];
        "Mod+Shift+V".action.switch-focus-between-floating-and-tiling = [ ];
        "Mod+W".action.toggle-column-tabbed-display = [ ];
      };
    };
  };
}
