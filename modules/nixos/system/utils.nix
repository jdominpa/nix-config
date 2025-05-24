{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.nixos.system.utils;
in
{
  options.jdp.nixos = {
    system.utils.enable = lib.mkEnableOption "Enable system management utility packages.";
  };

  config = lib.mkIf cfg.enable {
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
