{
  flake.nixosModules.latex =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.texliveFull ];
    };

  flake.darwinModules.latex =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.texliveFull ];
    };
}
