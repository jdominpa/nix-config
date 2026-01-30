{
  flake.modules.homeManager.niri = {
    programs.niri.settings = {
      window-rules = [
        {
          matches = [ { app-id = "^bitwarden$"; } ];
          block-out-from = "screencast";
        }
        {
          matches = [ { app-id = "^emacs$"; } ];
          open-maximized = true;
        }
        {
          matches = [ { app-id = "^steam$"; } ];
          open-maximized = true;
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
        {
          matches = [ { namespace = "^swaync-notification-window$"; } ];
          block-out-from = "screencast";
        }
      ];
    };
  };
}
