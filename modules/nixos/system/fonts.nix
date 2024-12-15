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
      enableDefaultPackages = false;
      fontDir.enable = true;
      packages = with pkgs; [
        font-awesome
        stable.iosevka-comfy.comfy
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-cjk-serif
        noto-fonts-emoji
      ];

      # User defined fonts
      fontconfig.defaultFonts = {
        monospace = [
          "Iosevka Comfy"
          "Noto Color Emoji"
        ];
        emoji = [ "Noto Color Emoji" ];
      };
    };
  };
}
