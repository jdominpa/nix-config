{
  flake.modules.homeManager.niri = {
    programs.niri.settings.input = {
      keyboard = {
        repeat-delay = 200;
        repeat-rate = 30;
        xkb = {
          layout = "us,us";
          options = "grp:shift_caps_toggle";
          variant = ",intl";
        };
      };
      mouse = {
        accel-profile = "flat";
        accel-speed = 0.2;
      };
      warp-mouse-to-focus.enable = true;
    };
  };
}
