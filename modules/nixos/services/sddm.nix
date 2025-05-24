{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.services.sddm;
in
{
  options.jdp.nixos = {
    services.sddm.enable = lib.mkEnableOption "Enable SDDM display manager.";
  };

  config = lib.mkIf cfg.enable {
    services.displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
    };
  };
}
