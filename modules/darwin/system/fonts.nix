{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.fonts;
in
{
  options.jdp.darwin = {
    system.fonts.enable = mkEnableOption "Enable fonts configuration.";
  };

  config = mkIf cfg.enable {
    fonts.packages = with pkgs; [
      stable.iosevka-comfy.comfy
      font-awesome
      (nerdfonts.override {
        fonts = [ "Iosevka" ];
      })
    ];
  };
}
