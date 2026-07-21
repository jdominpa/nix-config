{
  flake.modules.homeManager.niri = {
    programs.niri.settings = {
      window-rules = [
        {
          matches = [ { app-id = "^bitwarden$"; } ];
          block-out-from = "screen-capture";
        }
        {
          matches = [
            {
              app-id = "^emacs$";
            }
            {
              app-id = "^brave-browser$";
            }
            {
              app-id = "^steam$";
              title = "^Steam$";
            }
          ];
          open-maximized = true;
        }
        {
          matches = [ { app-id = "dev.noctalia.Noctalia"; } ];
          open-floating = true;
          default-column-width.fixed = 1080;
          default-window-height.fixed = 920;
        }
        {
          matches = [
            {
              app-id = "^steam$";
              title = "^Friends List$";
            }
            {
              app-id = "^steam$";
              title = "^Steam Settings$";
            }
            {
              app-id = "^thunar$";
            }
          ];
          open-floating = true;
        }
        {
          matches = [
            {
              app-id = "^gimp";
              title = "^GIMP Startup$";
            }
          ];
          open-focused = false;
        }
        {
          geometry-corner-radius = {
            bottom-left = 12.0;
            bottom-right = 12.0;
            top-left = 12.0;
            top-right = 12.0;
          };
          clip-to-geometry = true;
        }
      ];
      layer-rules = [
        # Rule for overview mode with noctalia
        {
          matches = [ { namespace = "^noctalia-wallpaper"; } ];
          place-within-backdrop = true;
        }
        {
          matches = [ { namespace = "^swaync-notification-window$"; } ];
          block-out-from = "screencast";
        }
      ];
    };
  };
}
