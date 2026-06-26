{
  flake.modules.nixos.login-manager = {
    services.displayManager.gdm.enable = true;
  };
}
