{
  flake.nixosModules.discord =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.vesktop ];
    };

  flake.darwinModules.discord =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.vesktop ];
    };
}
