{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.system.locale;
in
{
  options.jdp.darwin = {
    system.locale.enable = lib.mkEnableOption "Enable locale and timezone settings.";
  };

  config = lib.mkIf cfg.enable {
    time.timeZone = "Europe/Madrid";
    system.defaults.menuExtraClock.Show24Hour = true; # show 24 hour format
  };
}
