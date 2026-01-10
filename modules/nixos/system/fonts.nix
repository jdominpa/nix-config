{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.nixos.system.fonts;
in
{
  options.jdp.nixos = {
    system.fonts.enable = lib.mkEnableOption "Enable fonts configuration.";
  };

  config = lib.mkIf cfg.enable {
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
}
