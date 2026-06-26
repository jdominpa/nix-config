{
  inputs,
  self,
  ...
}:
{
  flake.modules.nixos.noctalia = {
    imports = [ inputs.noctalia.nixosModules.default ];
    home-manager.sharedModules = [ self.modules.homeManager.noctalia ];
  };

  flake.modules.homeManager.noctalia = {
    imports = [ inputs.noctalia.homeModules.default ];

    # Noctalia settings
    programs.noctalia = {
      enable = true;
      settings = {
        bar = {
          widgets = {
            start = [
              "session"
              "launcher"
              "control-center"
              "spacer_0"
              "media"
            ];
            center = [ "workspaces" ];
            end = [
              "tray"
              "spacer_1"
              "keyboard_layout"
              "notifications"
              "clipboard"
              "network"
              "bluetooth"
              "volume"
              "brightness"
              "battery"
              "spacer_0"
              "clock"
            ];
            margin_edge = 5;
            margin_ends = 5;
            padding = 10;
          };
        };
        idle = {
          behavior = {
            lock = {
              action = "lock";
              enabled = true;
              timeout = 900.0;
            };
            lock-and-suspend = {
              action = "lock_and_suspend";
              enabled = false;
            };
            screen-off = {
              action = "screen_off";
              enabled = true;
              timeout = 1200.0;
            };
          };
          behavior_order = [
            "lock"
            "screen-off"
            "lock-and-suspend"
          ];
        };
        keybinds.validate = [
          "Return"
          "KP_Enter"
        ];
        location.auto_locate = true;
        nightlight.enabled = true;
        shell = {
          clipboard_history_max_entries = 10;
          panel.open_near_click_control_center = true;
        };

        # TODO: replace with stylix
        shell.font_family = "Aporetic Sans";
        theme = {
          mode = "dark";
        };
        wallpaper.directory = "~/Imatges/Wallpapers";

        widget = {
          bluetooth.hide_when_no_connected_device = true;
          clock.format = "{:%H:%M %d-%m-%Y}";
          spacer_0 = {
            length = 15;
            type = "spacer";
          };
          spacer_1 = {
            length = 10;
            type = "spacer";
          };
          volume.scroll_step = 2;
        };
      };
    };
  };
}
