{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    plasma.enable = lib.mkEnableOption "Enable and configure the KDE Plasma desktop environment.";
  };

  config = lib.mkIf config.plasma.enable {
    services.desktopManager = {
      plasma6.enable = true;
    };
    environment.plasma6.excludePackages = with pkgs.kdePackages; [
      kate
    ];
  };
}
