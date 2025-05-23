{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.system.boot.systemd;
in
{
  options.jdp.nixos = {
    system.boot.systemd.enable = lib.mkEnableOption "Enable systemd bootloader.";
  };

  config = lib.mkIf cfg.enable {
    boot.loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        enable = true;
        # Don't keep to many generations
        configurationLimit = 10;
        # Pick the highest resoltion for systemd-boot's console
        consoleMode = "max";
      };
    };
  };
}
