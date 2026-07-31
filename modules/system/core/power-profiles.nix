{
  flake.modules.nixos.power-profiles = {
    services = {
      power-profiles-daemon.enable = true;
      upower.enable = true;
    };
  };

  flake.modules.darwin.power-profiles = {
    power = {
      restartAfterFreeze = true;
      sleep.display = 15;
    };
  };
}
