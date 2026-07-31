{
  flake.modules.nixos.locale = {
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
    time.timeZone = "Europe/Madrid";
  };

  flake.modules.darwin.locale = {
    time.timeZone = "Europe/Madrid";
    system.defaults.menuExtraClock.Show24Hour = true; # show 24 hour format
  };
}
