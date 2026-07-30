{
  flake.nixosModules.power-profiles = {
    services = {
      power-profiles-daemon.enable = true;
      upower.enable = true;
    };
  };

  flake.darwinModules.power-profiles = {
    power = {
      restartAfterFreeze = true;
      sleep.display = 15;
    };
  };
}
