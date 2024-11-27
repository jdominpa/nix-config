{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.desktop.plasma-manager;
  user = config.jdp.base.user;
in
{
  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.plasma = {
        powerdevil = {
          AC = {
            autoSuspend.action = "nothing";
            dimDisplay.enable = false;
            turnOffDisplay = {
              idleTimeout = 1200;
              idleTimeoutWhenLocked = 60;
            };
            powerButtonAction = "showLogoutScreen";
            powerProfile = "performance";
          };
          general.pausePlayersOnSuspend = true;
        };
      };
    };
  };
}
