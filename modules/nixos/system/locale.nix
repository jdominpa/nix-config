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
        LC_ADDRESS = "ca_ES.UTF-8";
        LC_IDENTIFICATION = "ca_ES.UTF-8";
        LC_MEASUREMENT = "ca_ES.UTF-8";
        LC_MONETARY = "ca_ES.UTF-8";
        LC_NAME = "ca_ES.UTF-8";
        LC_NUMERIC = "ca_ES.UTF-8";
        LC_PAPER = "ca_ES.UTF-8";
        LC_TELEPHONE = "ca_ES.UTF-8";
        LC_TIME = "ca_ES.UTF-8";
      };
    };
    time.timeZone = cfg.timezone;
  };
}
