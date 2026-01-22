{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.apps.kitty;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    apps.kitty.enable = lib.mkEnableOption "Whether to configure Kitty terminal.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.kitty = {
        enable = true;
        font = {
          name = "Aporetic Sans Mono";
          size = 13;
        };
        settings = {
          cursor_shape = "block";
          cursor_blink_interval = 0;
          tab_bar_edge = "top";
          tab_bar_style = "slant";
        };
        themeFile = "Modus_Vivendi";
      };
    };
  };
}
