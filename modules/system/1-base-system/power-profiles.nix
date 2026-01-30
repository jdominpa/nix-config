{
  flake.modules.nixos.powerProfiles = {
    services = {
      power-profiles-daemon.enable = true;
      upower.enable = true;
    };
  };

  flake.modules.darwin.powerProfiles = {
    power = {
      restartAfterFreeze = true;
      sleep.display = 15;
    };
  };
}
