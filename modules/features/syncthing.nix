{
  flake.modules.nixos.syncthing =
    { pkgs, ... }:
    {
      services.syncthing = {
        enable = true;
        user = "jdominpa";
        group = "users";
        configDir = "/home/jdominpa/.config/syncthing";
        dataDir = "/home/jdominpa";
        openDefaultPorts = true;
      };
      environment.systemPackages = [ pkgs.syncthingtray ];
    };

  flake.modules.darwin.syncthing = {
    homebrew.casks = [ "syncthing-app" ];
  };
}
