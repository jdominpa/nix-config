{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.services.power;
in
{
  options.jdp.nixos = {
    services.power.enable = mkEnableOption "Enable power profiles.";
  };

  config = mkIf cfg.enable {
    services = {
      power-profiles-daemon.enable = true;
      upower.enable = true;
    };
  };
}
