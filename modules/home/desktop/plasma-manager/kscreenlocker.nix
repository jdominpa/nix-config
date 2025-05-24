{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.plasma-manager;
  inherit (config.jdp.base) user;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.plasma.kscreenlocker = {
        appearance = {
          alwaysShowClock = true;
          showMediaControls = false;
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
