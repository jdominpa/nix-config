{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.desktop.plasma-manager;
  inherit (config.jdp.base) user;
in
{
  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.plasma = {
        powerdevil = {
          AC = {
            autoSuspend.action = "nothing";
            dimDisplay.enable = false;
            turnOffDisplay.idleTimeout = "never";
            powerButtonAction = "showLogoutScreen";
            powerProfile = "performance";
          };
          general.pausePlayersOnSuspend = true;
        };
      };
    };
  };
}
