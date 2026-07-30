{
  flake.nixosModules.discord =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.vencord ];
    };

  flake.darwinModules.discord =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.vencord ];
    };
}
