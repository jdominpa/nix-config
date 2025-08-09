{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.system.locale;
in
{
  options.jdp.nixos = {
    system.locale = {
      enable = lib.mkEnableOption "Enable locale and timezone settings.";
      timezone = lib.mkOption {
        type = lib.types.str;
        default = "Europe/Madrid";
        description = "Timezone location";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    i18n = {
      defaultLocale = "ca_ES.UTF-8";
      extraLocaleSettings = {
        LC_ADDRESS = "ca_ES.UTF-8";
        LC_IDENTIFICATION = "ca_ES.UTF-8";
        LC_MEASUREMENT = "ca_ES.UTF-8";
        LC_MONETARY = "ca_ES.UTF-8";
        LC_NAME = "ca_ES.UTF-8";
        LC_PAPER = "ca_ES.UTF-8";
        LC_TELEPHONE = "ca_ES.UTF-8";
        LC_TIME = "ca_ES.UTF-8";
      };
    };
    time.timeZone = cfg.timezone;
  };
}
