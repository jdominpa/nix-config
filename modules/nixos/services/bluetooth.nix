{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.services.bluetooth;
in
{
  options.jdp.nixos = {
    services.bluetooth.enable = lib.mkEnableOption "Enable bluetooth.";
  };

  config = lib.mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };
}
