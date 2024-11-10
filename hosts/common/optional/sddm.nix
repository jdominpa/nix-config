{
  config,
  lib,
  ...
}: {
  options = {
    sddm.enable = lib.mkEnableOption "Enable and configure the KDE Plasma desktop environment.";
  };

  config = lib.mkIf config.sddm.enable {
    services.displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
      sddm.wayland.compositor = "kwin";
    };
  };
}
