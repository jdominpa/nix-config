{pkgs, ...}: {
  # System packages specific to NixOS
  environment.systemPackages = with pkgs; [
    lm_sensors
    pciutils
    usbutils
  ];
}
