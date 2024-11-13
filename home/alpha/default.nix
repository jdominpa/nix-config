{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ../common
  ];

  # KDE Plasma configuration
  plasma-manager.enable = true;
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

  home.packages = with pkgs; [
    brave
    discord
  ];

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "24.05";
}
