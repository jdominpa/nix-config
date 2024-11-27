{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.desktop.plasma-manager;
  user = config.jdp.base.user;
  plasma = config.home-manager.users.${user.name}.programs.plasma;
in
{
  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.plasma.kscreenlocker = {
        appearance = {
          alwaysShowClock = true;
          showMediaControls = false;
          inherit (plasma.workspace) wallpaperPlainColor;
        };
        autoLock = true;
        lockOnResume = true;
        passwordRequired = true;
        passwordRequiredDelay = 10;
        timeout = 20;
      };
    };
  };
}
