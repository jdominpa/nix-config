let
  fonts =
    { pkgs, ... }:
    {
      fonts.packages = with pkgs; [
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-cjk-serif
        noto-fonts-color-emoji
        aporetic
      ];
    };
in
{
  flake.modules.nixos.fonts = {
    imports = [ fonts ];
  };

  flake.modules.darwin.fonts = {
    imports = [ fonts ];
  };
}
