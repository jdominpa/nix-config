{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.darwin.system.fonts;
in
{
  options.jdp.darwin = {
    system.fonts.enable = lib.mkEnableOption "Enable fonts configuration.";
  };

  config = lib.mkIf cfg.enable {
    fonts.packages = [ pkgs.aporetic ];
  };
}
