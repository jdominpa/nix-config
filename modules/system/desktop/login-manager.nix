{
  flake.modules.nixos.login-manager = {
    services.displayManager.noctalia-greeter.enable = true;
  };
}
