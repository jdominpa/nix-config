{
  config,
  self,
  ...
}:
let
  inherit (config.user) homeDirectory;
in
{
  flake.modules.nixos.noctalia =
    { pkgs, ... }:
    {
      programs.noctalia = {
        enable = true;
        package = self.wrappers.noctalia.wrap { inherit pkgs; };
        systemd.enable = true;
      };
    };

  flake.wrappers.noctalia =
    { wlib, ... }:
    {
      imports = [ wlib.wrapperModules.noctalia ];
      settings = {
        bar = {
          default = {
            center = [
              "workspaces"
            ];
            end = [
              "tray"
              "notifications"
              "clipboard"
              "keyboard_layout"
              "network"
              "bluetooth"
              "volume"
              "brightness"
              "battery"
              "clock"
            ];
            margin_ends = 0;
            radius = 0;
            start = [
              "session"
              "launcher"
              "wallpaper"
              "media"
            ];
            widget_spacing = 10;
          };
        };
        idle = {
          behavior = {
            lock = {
              action = "lock";
              enabled = true;
              timeout = 600.0;
            };
            screen-off = {
              action = "screen_off";
              enabled = true;
              timeout = 660.0;
            };
          };
          behavior_order = [
            "lock"
            "screen-off"
          ];
        };
        keybinds = {
          validate = [
            "Return"
            "KP_Enter"
          ];
        };
        location.auto_locate = true;
        nightlight.enabled = true;
        shell = {
          clipboard_history_max_entries = 10;
          greeter_sync.auto_sync = true;
          launch_apps_as_systemd_services = true;
          panel = {
            open_near_click_clipboard = true;
            open_near_click_control_center = true;
          };
          polkit_agent = true;
        };
        theme = {
          source = "wallpaper";
          templates.builtin_ids = [
            "gtk3"
            "gtk4"
            "niri"
          ];
        };
        wallpaper.directory = "${homeDirectory.linux}/Imatges/Wallpapers";
        widget = {
          bluetooth = {
            hide_when_no_connected_device = true;
          };
          clock = {
            format = "{:%a %d %b %H:%M}";
          };
          media = {
            hide_when_no_media = true;
          };
          network = {
            show_label = false;
          };
          notifications = {
            hide_when_no_unread = true;
          };
          tray = {
            drawer = true;
          };
          volume = {
            show_label = false;
          };
        };
      };
    };
}
