{
  flake.modules.nixos.bluetooth = {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = false;
      # Shorter pairing/discovery windows and private addresses
      settings.General = {
        PairableTimeout = 30;
        DiscoverableTimeout = 30;
        Privacy = "device";
      };
    };
  };
}
