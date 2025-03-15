{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.desktop.plasma;
in
{
  options.jdp.nixos = {
    desktop.plasma.enable = mkEnableOption "Enable KDE Plasma desktop environment.";
  };

  config = mkIf cfg.enable {
    services.desktopManager = {
      plasma6.enable = true;
    };
    environment.plasma6.excludePackages = with pkgs.kdePackages; [
      kate
      kwallet
      kwalletmanager
    ];
  };
}
