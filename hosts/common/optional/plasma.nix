{
  config,
  lib,
  ...
}: {
  options = {
    plasma.enable = lib.mkEnableOption "Enable and configure the KDE Plasma desktop environment.";
  };

  config = lib.mkIf config.plasma.enable {
    services.displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
      sddm.wayland.compositor = "kwin";
    };
    services.desktopManager = {
      plasma6.enable = true;
    };
  };
}
