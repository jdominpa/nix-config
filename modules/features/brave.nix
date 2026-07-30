{
  flake.nixosModules.brave =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.brave ];
    };

  flake.darwinModules.brave =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.brave ];
    };
}
