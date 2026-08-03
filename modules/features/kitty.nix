let
  install = {
    wrappers.kitty.enable = true;
  };
in
{
  flake.wrappers.kitty =
    {
      config,
      lib,
      pkgs,
      wlib,
      ...
    }:
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
      # Copy the wrapped binary correctly to /Applications on darwin
      wrapperImplementation = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin "binary";
      buildCommand.kittyAppBundle = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
        after = [
          "symlinkScript"
          "makeWrapper"
        ];
        data = ''
          bundleExe=${placeholder config.outputName}/Applications/kitty.app/Contents/MacOS/kitty
          rm -f "$bundleExe"
          cp ${config.wrapperPaths.placeholder} "$bundleExe"
        '';
      };
    };

  flake.modules.nixos.kitty = install;

  flake.modules.darwin.kitty = install;
}
