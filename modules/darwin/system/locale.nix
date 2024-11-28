{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.locale;
in
{
  options.jdp.darwin = {
    system.locale.enable = mkEnableOption "Enable locale and timezone settings.";
  };

  config = mkIf cfg.enable {
    time.timeZone = "Europe/Madrid";
    system.defaults.menuExtraClock.Show24Hour = true; # show 24 hour format
  };
}
