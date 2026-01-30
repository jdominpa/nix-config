{
  flake.modules.nixos.fonts =
    { pkgs, ... }:
    {
      fonts = {
        enableDefaultPackages = true;
        enableGhostscriptFonts = true;
        fontDir.enable = true;
        packages = with pkgs; [
          aporetic
          noto-fonts-color-emoji
        ];

        # User defined fonts
        fontconfig.defaultFonts = {
          serif = [ "Aporetic Serif" ];
          sansSerif = [ "Aporetic Sans" ];
          monospace = [
            "Aporetic Sans Mono"
            "Noto Color Emoji"
          ];
          emoji = [ "Noto Color Emoji" ];
        };
      };
    };

  flake.modules.darwin.fonts =
    { pkgs, ... }:
    {
      fonts.packages = [ pkgs.aporetic ];
    };
}
