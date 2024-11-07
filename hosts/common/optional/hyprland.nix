{
  config,
  lib,
  ...
}: {
  options = {
    hyprland.enable = lib.mkEnableOption "Enable and configure the Hyprland compositor.";
  };

  config = lib.mkIf config.hyprland.enable {
    programs.hyprland.enable = true;
  };
}
