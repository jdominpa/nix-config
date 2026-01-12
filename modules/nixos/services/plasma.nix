{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.nixos.services.plasma;
in
{
  options.jdp.nixos = {
    services.plasma.enable = lib.mkEnableOption "Enable KDE Plasma desktop environment.";
  };

  config = lib.mkIf cfg.enable {
    services.desktopManager = {
      plasma6.enable = true;
    };
    environment.plasma6.excludePackages = with pkgs.kdePackages; [
      kate
    ];
  };
}
