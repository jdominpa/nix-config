{
  flake.wrappers.niri.settings = {
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
        matches = [
          { app-id = "xdg-desktop-portal-gtk"; }
        ];
        open-floating = true;
        default-column-width.fixed = 1080;
        default-window-height.fixed = 920;
      }
      {
        matches = [
          { app-id = "xdg-desktop-portal-gtk"; }
        ];
        open-floating = true;
        default-column-width.fixed = 1280;
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
        # niri takes the radius as arguments, not as a block, so the four equal
        # corners collapse into the single-argument form.
        geometry-corner-radius = 12.0;
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
}
