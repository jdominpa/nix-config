{
  flake.nixosModules.syncthing =
    { pkgs, ... }:
    {
      services.syncthing.enable = true;
      environment.systemPackages = [ pkgs.syncthingtray ];
    };

  flake.darwinModules.syncthing = {
    homebrew.casks = [ "syncthing-app" ];
  };
}
