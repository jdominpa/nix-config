{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.system.utils;
in
{
  options.jdp.nixos = {
    system.utils.enable = mkEnableOption "Enable system management utility packages.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      btop
      exfat # ExFAT drives
      hfsprogs # macOS drives
      lm_sensors
      ntfs3g # Windows drives
      pciutils
      usbutils
    ];
  };
}
