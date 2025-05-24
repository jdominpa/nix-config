{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.system.fonts;
in
{
  options.jdp.nixos = {
    system.fonts.enable = mkEnableOption "Enable fonts configuration.";
  };

  config = mkIf cfg.enable {
    fonts = {
      enableDefaultPackages = true;
      enableGhostscriptFonts = true;
      fontDir.enable = true;
      packages = with pkgs; [
        aporetic
        noto-fonts-emoji
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
