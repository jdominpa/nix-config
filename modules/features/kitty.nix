{
  config,
  ...
}:
let
  install = {
    imports = [ config.flake.wrappers.kitty.install ];
    wrappers.kitty.enable = true;
  };
in
{
  flake.wrappers.kitty =
    { wlib, ... }:
    {
      imports = [ wlib.wrapperModules.kitty ];
      font = {
        name = "Aporetic Sans Mono";
        size = 13;
      };
      settings = {
        confirm_os_window_close = 2;
        cursor_blink_interval = 0;
        cursor_shape = "block";
        tab_bar_edge = "top";
        tab_bar_style = "slant";
      };
      themeFile = "Modus_Vivendi";
    };

  flake.modules.nixos.kitty = install;

  flake.modules.darwin.kitty = install;
}
