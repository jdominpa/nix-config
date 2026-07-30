{
  inputs,
  moduleWithSystem,
  ...
}:
{
  flake.nixosModules.kitty = moduleWithSystem (
    { self', ... }: {
      environment.systemPackages = [ self'.packages.kitty ];
    }
  );

  flake.darwinModules.kitty = moduleWithSystem (
    { self', ... }: {
      environment.systemPackages = [ self'.packages.kitty ];
    }
  );

  perSystem = { pkgs, ... }: {
    packages.kitty = inputs.wrappers.wrappers.kitty.wrap {
      inherit pkgs;
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
  };
}
