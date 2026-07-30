{
  flake.nixosModules.login-manager = {
    services.displayManager.gdm.enable = true;
  };
}
