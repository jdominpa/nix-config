{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.power;
in
{
  options.jdp.darwin = {
    system.power.enable = mkEnableOption "Enable power settings.";
  };

  config = mkIf cfg.enable {
    power = {
      restartAfterFreeze = true;
      sleep.display = 15;
    };
  };
}
