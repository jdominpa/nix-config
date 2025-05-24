{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.services.power;
in
{
  options.jdp.nixos = {
    services.power.enable = lib.mkEnableOption "Enable power profiles.";
  };

  config = lib.mkIf cfg.enable {
    services = {
      power-profiles-daemon.enable = true;
      upower.enable = true;
    };
  };
}
