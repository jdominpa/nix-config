{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.system.power;
in
{
  options.jdp.darwin = {
    system.power.enable = lib.mkEnableOption "Enable power settings.";
  };

  config = lib.mkIf cfg.enable {
    power = {
      restartAfterFreeze = true;
      sleep.display = 15;
    };
  };
}
