{inputs, ...}: {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    ./jdominpa.nix
  ];
}
