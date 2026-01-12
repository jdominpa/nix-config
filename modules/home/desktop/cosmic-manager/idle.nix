{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.cosmic-manager;
  inherit (config.jdp.base) user;
  inherit (config.home-manager.users.${user.name}.lib.cosmic) mkRON;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      wayland.desktopManager.cosmic.idle = {
        screen_off_time = mkRON "optional" 900000;
        suspend_on_ac_time = mkRON "optional" null;
        suspend_on_battery_time = mkRON "optional" 1200000;
      };
    };
  };
}
