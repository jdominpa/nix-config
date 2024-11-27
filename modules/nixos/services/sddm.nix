{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.services.sddm;
in
{
  options.jdp.nixos = {
    services.sddm.enable = mkEnableOption "Enable SDDM display manager.";
  };

  config = mkIf cfg.enable {
    services.displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
    };
  };
}
