{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.desktop.gdm;
in
{
  options.jdp.nixos = {
    desktop.gdm.enable = lib.mkEnableOption "Enable GDM display manager.";
  };

  config = lib.mkIf cfg.enable {
    services.displayManager.gdm.enable = true;
  };
}
