{lib, ...}: {
  boot.loader.systemd-boot = {
    # Don't keep to many generations
    configurationLimit = 10;
    # Pick the highest resoltion for systemd-boot's console
    consoleMode = "max";
  };
  services = {
    power-profiles-daemon.enable = true;
    upower.enable = true;
  };
}
