{
  flake.nixosModules.ssh = {
    services.openssh.enable = true;
  };

  flake.darwinModules.ssh = {
    services.openssh.enable = true;
  };
}
