{
  self,
  ...
}:
{
  flake.wrappers.niri =
    { lib, pkgs, ... }:
    let
      noctaliaExe = lib.getExe (self.wrappers.noctalia-shell.wrap { inherit pkgs; });
    in
    {
      settings.binds = {
        "Mod+Shift+Slash".show-hotkey-overlay = _: { };
        "Mod+T" = _: {
          props.hotkey-overlay-title = "Open a terminal";
          content.spawn = "kitty";
        };
        "Mod+Space" = _: {
          props.hotkey-overlay-title = "Run application launcher";
          content.spawn = [
            "${noctaliaExe}"
            "ipc"
            "call"
            "launcher"
            "toggle"
          ];
        };
        "Mod+P".screenshot = _: { };
        "Mod+Ctrl+P".screenshot-window = _: { };
        "Mod+Alt+P".screenshot-screen = _: { };
        "Mod+Escape".toggle-keyboard-shortcuts-inhibit = _: { };
        "Mod+Alt+L" = _: {
          props.hotkey-overlay-title = "Lock the screen";
          content.spawn = [
            "${noctaliaExe}"
            "ipc"
            "lockScreen"
            "lock"
          ];
        };
        "Mod+Shift+Z".power-off-monitors = _: { };
        "Mod+Shift+Q".quit = _: { };
        "Ctrl+Alt+Delete".quit = _: { };

        # Media and brightness bindings
        "XF86AudioRaiseVolume" = _: {
          props.allow-when-locked = true;
          content.spawn = [
            "wpctl"
            "set-volume"
            "@DEFAULT_AUDIO_SINK@"
            "0.01+"
          ];
        };
        "XF86AudioLowerVolume" = _: {
          props.allow-when-locked = true;
          content.spawn = [
            "wpctl"
            "set-volume"
            "@DEFAULT_AUDIO_SINK@"
            "0.01-"
          ];
        };
        "XF86AudioMute" = _: {
          props = {
            allow-when-locked = true;
            repeat = false;
          };
          content.spawn = [
            "wpctl"
            "set-mute"
            "@DEFAULT_AUDIO_SINK@"
            "toggle"
          ];
        };
        "XF86AudioMicMute" = _: {
          props = {
            allow-when-locked = true;
            repeat = false;
          };
          content.spawn = [
            "wpctl"
            "set-mute"
            "@DEFAULT_AUDIO_SOURCE@"
            "toggle"
          ];
        };
        "XF86AudioPlay" = _: {
          props = {
            allow-when-locked = true;
            repeat = false;
          };
          content.spawn = [
            "playerctl"
            "play-pause"
          ];
        };
        "XF86AudioStop" = _: {
          props = {
            allow-when-locked = true;
            repeat = false;
          };
          content.spawn = [
            "playerctl"
            "stop"
          ];
        };
        "XF86AudioPrev" = _: {
          props = {
            allow-when-locked = true;
            repeat = false;
          };
          content.spawn = [
            "playerctl"
            "previous"
          ];
        };
        "XF86AudioNext" = _: {
          props = {
            allow-when-locked = true;
            repeat = false;
          };
          content.spawn = [
            "playerctl"
            "next"
          ];
        };
        "XF86MonBrightnessUp" = _: {
          props.allow-when-locked = true;
          content.spawn = [
            "brightnessctl"
            "--class=backlight"
            "set"
            "+10%"
          ];
        };
        "XF86MonBrightnessDown" = _: {
          props.allow-when-locked = true;
          content.spawn = [
            "brightnessctl"
            "--class=backlight"
            "set"
            "10%-"
          ];
        };

        # Navigation bindings
        "Mod+F" = _: {
          props.repeat = false;
          content.toggle-overview = _: { };
        };
        "Mod+Left".focus-column-left = _: { };
        "Mod+Down".focus-window-down = _: { };
        "Mod+Up".focus-window-up = _: { };
        "Mod+Right".focus-column-right = _: { };
        "Mod+J".focus-column-left = _: { };
        "Mod+K".focus-window-down = _: { };
        "Mod+L".focus-window-up = _: { };
        "Mod+Semicolon".focus-column-right = _: { };
        "Mod+WheelScrollLeft".focus-column-left = _: { };
        "Mod+WheelScrollRight".focus-column-right = _: { };

        "Mod+Home".focus-column-first = _: { };
        "Mod+End".focus-column-last = _: { };

        "Mod+Ctrl+Left".focus-monitor-left = _: { };
        "Mod+Ctrl+Down".focus-monitor-down = _: { };
        "Mod+Ctrl+Up".focus-monitor-up = _: { };
        "Mod+Ctrl+Right".focus-monitor-right = _: { };
        "Mod+Ctrl+J".focus-monitor-left = _: { };
        "Mod+Ctrl+K".focus-monitor-down = _: { };
        "Mod+Ctrl+L".focus-monitor-up = _: { };
        "Mod+Ctrl+Semicolon".focus-monitor-right = _: { };

        "Mod+I".focus-workspace-down = _: { };
        "Mod+O".focus-workspace-up = _: { };
        "Mod+WheelScrollDown" = _: {
          props.cooldown-ms = 150;
          content.focus-workspace-down = _: { };
        };
        "Mod+WheelScrollUp" = _: {
          props.cooldown-ms = 150;
          content.focus-workspace-up = _: { };
        };

        "Mod+1".focus-workspace = 1;
        "Mod+2".focus-workspace = 2;
        "Mod+3".focus-workspace = 3;
        "Mod+4".focus-workspace = 4;
        "Mod+5".focus-workspace = 5;
        "Mod+6".focus-workspace = 6;
        "Mod+7".focus-workspace = 7;
        "Mod+8".focus-workspace = 8;
        "Mod+9".focus-workspace = 9;

        # Window moving bindings
        "Mod+Q" = _: {
          props.repeat = false;
          content.close-window = _: { };
        };
        "Mod+Shift+Left".move-column-left = _: { };
        "Mod+Shift+Down".move-window-down = _: { };
        "Mod+Shift+Up".move-window-up = _: { };
        "Mod+Shift+Right".move-column-right = _: { };
        "Mod+Shift+J".move-column-left = _: { };
        "Mod+Shift+K".move-window-down = _: { };
        "Mod+Shift+L".move-window-up = _: { };
        "Mod+Shift+Semicolon".move-column-right = _: { };
        "Mod+Shift+WheelScrollLeft".move-column-left = _: { };
        "Mod+Shift+WheelScrollRight".move-column-right = _: { };

        "Mod+Shift+Home".move-column-to-first = _: { };
        "Mod+Shift+End".move-column-to-last = _: { };

        "Mod+Ctrl+Shift+Left".move-column-to-monitor-left = _: { };
        "Mod+Ctrl+Shift+Down".move-column-to-monitor-down = _: { };
        "Mod+Ctrl+Shift+Up".move-column-to-monitor-up = _: { };
        "Mod+Ctrl+Shift+Right".move-column-to-monitor-right = _: { };
        "Mod+Ctrl+Shift+J".move-column-to-monitor-left = _: { };
        "Mod+Ctrl+Shift+K".move-column-to-monitor-down = _: { };
        "Mod+Ctrl+Shift+L".move-column-to-monitor-up = _: { };
        "Mod+Ctrl+Shift+Semicolon".move-column-to-monitor-right = _: { };

        "Mod+Shift+I".move-column-to-workspace-down = _: { };
        "Mod+Shift+O".move-column-to-workspace-up = _: { };
        "Mod+Ctrl+Shift+I".move-workspace-down = _: { };
        "Mod+Ctrl+Shift+O".move-workspace-up = _: { };
        "Mod+Shift+WheelScrollDown" = _: {
          props.cooldown-ms = 150;
          content.move-workspace-down = _: { };
        };
        "Mod+Shift+WheelScrollUp" = _: {
          props.cooldown-ms = 150;
          content.move-workspace-up = _: { };
        };

        "Mod+Shift+1".move-column-to-workspace = 1;
        "Mod+Shift+2".move-column-to-workspace = 2;
        "Mod+Shift+3".move-column-to-workspace = 3;
        "Mod+Shift+4".move-column-to-workspace = 4;
        "Mod+Shift+5".move-column-to-workspace = 5;
        "Mod+Shift+6".move-column-to-workspace = 6;
        "Mod+Shift+7".move-column-to-workspace = 7;
        "Mod+Shift+8".move-column-to-workspace = 8;
        "Mod+Shift+9".move-column-to-workspace = 9;

        "Mod+Comma".consume-window-into-column = _: { };
        "Mod+Period".expel-window-from-column = _: { };

        # Window management bindings
        "Mod+R".switch-preset-column-width = _: { };
        "Mod+Shift+R".switch-preset-window-height = _: { };
        "Mod+Ctrl+R".reset-window-height = _: { };
        "Mod+M".maximize-column = _: { };
        "Mod+Shift+M".fullscreen-window = _: { };
        "Mod+Ctrl+M".expand-column-to-available-width = _: { };
        "Mod+Alt+M".maximize-window-to-edges = _: { };
        "Mod+C".center-column = _: { };
        "Mod+Minus".set-column-width = "-10%";
        "Mod+Equal".set-column-width = "+10%";
        "Mod+Ctrl+Minus".set-column-width = "-1";
        "Mod+Ctrl+Equal".set-column-width = "+1";
        "Mod+Shift+Minus".set-window-height = "-10%";
        "Mod+Shift+Equal".set-window-height = "+10%";
        "Mod+Ctrl+Shift+Minus".set-window-height = "-1";
        "Mod+Ctrl+Shift+Equal".set-window-height = "+1";
        "Mod+V".toggle-window-floating = _: { };
        "Mod+Shift+V".switch-focus-between-floating-and-tiling = _: { };
        "Mod+W".toggle-column-tabbed-display = _: { };
      };
    };
}
