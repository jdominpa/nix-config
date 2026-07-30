let
  fonts =
    { pkgs, ... }:
    {
      fonts.packages = with pkgs; [
        aporetic
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-cjk-serif
        noto-fonts-color-emoji
      ];
    };
in
{
  flake.nixosModules.fonts = {
    imports = [ fonts ];
    fonts.fontconfig.defaultFonts = {
      serif = [ "Aporetic Serif" ];
      sansSerif = [ "Aporetic Sans" ];
      monospace = [ "Aporetic Sans Mono" ];
      emoji = [ "Noto Color Emoji" ];
    };
  };

  flake.darwinModules.fonts = {
    imports = [ fonts ];
  };
}
