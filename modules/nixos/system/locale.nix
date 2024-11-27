{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.system.locale;
in
{
  options.jdp.nixos = {
    system.locale = {
      enable = mkEnableOption "Enable locale and timezone settings.";
      timezone = mkOption {
        type = types.str;
        default = "Europe/Madrid";
        description = "Timezone location";
      };
    };
  };

  config = mkIf cfg.enable {
    i18n = {
      defaultLocale = "en_US.UTF-8";
      extraLocaleSettings = {
        LC_ADDRESS = "es_ES.UTF-8";
        LC_IDENTIFICATION = "es_ES.UTF-8";
        LC_MEASUREMENT = "es_ES.UTF-8";
        LC_MONETARY = "es_ES.UTF-8";
        LC_NAME = "es_ES.UTF-8";
        LC_NUMERIC = "es_ES.UTF-8";
        LC_PAPER = "es_ES.UTF-8";
        LC_TELEPHONE = "es_ES.UTF-8";
        LC_TIME = "es_ES.UTF-8";
      };
    };
    time.timeZone = cfg.timezone;
  };
}
