{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.desktop.sddm;
in
{
  options.jdp.nixos = {
    desktop.sddm.enable = lib.mkEnableOption "Enable SDDM display manager.";
  };

  config = lib.mkIf cfg.enable {
    services.displayManager.sddm.enable = true;
  };
}
