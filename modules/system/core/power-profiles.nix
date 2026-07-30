{
  flake.nixosModules.powerProfiles = {
    services = {
      power-profiles-daemon.enable = true;
      upower.enable = true;
    };
  };

  flake.darwinModules.powerProfiles = {
    power = {
      restartAfterFreeze = true;
      sleep.display = 15;
    };
  };
}
