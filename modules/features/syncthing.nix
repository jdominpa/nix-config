{
  flake.modules.nixos.syncthing = {
    services.syncthing = {
      enable = true;
      user = "jdominpa";
      group = "users";
      configDir = "/home/jdominpa/.config/syncthing";
      dataDir = "/home/jdominpa";
      openDefaultPorts = true;
    };
  };

  flake.modules.darwin.syncthing = {
    homebrew.casks = [ "syncthing-app" ];
  };
}
