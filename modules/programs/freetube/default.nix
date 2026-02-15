{
  flake.modules.nixos.freetube =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.freetube ];
    };

  flake.modules.darwin.freetube =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.freetube ];
    };
}
