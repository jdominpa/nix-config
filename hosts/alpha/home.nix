{pkgs, ...}: {
  modules.plasma-manager.enable = true;
  programs.plasma = {
    input.mice = [
      {
        name = "Logitech G502 HERO Gaming Mouse";
        enable = true;
        accelerationProfile = "none";
        productId = "c08b";
        vendorId = "046d";
      }
    ];
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
}
