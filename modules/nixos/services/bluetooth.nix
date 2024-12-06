{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.services.bluetooth;
in
{
  options.jdp.nixos = {
    services.bluetooth.enable = mkEnableOption "Enable bluetooth.";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };
}
