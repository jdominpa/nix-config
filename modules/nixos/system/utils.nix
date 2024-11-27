{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.system.utils;
in
{
  options.jdp.nixos = {
    system.utils.enable = mkEnableOption "Enable system management utility packages.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      lm_sensors
      pciutils
      usbutils
    ];
  };
}
