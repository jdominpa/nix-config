{
  flake.modules.nixos.ssh = {
    services.openssh = {
      enable = true;
    };
  };

  flake.modules.darwin.ssh = {
    services.openssh = {
      enable = true;
    };
  };
}
