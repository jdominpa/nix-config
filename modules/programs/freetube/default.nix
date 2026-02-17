let
  freetube =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.freetube ];
    };
in
{
  flake.modules.nixos.freetube = freetube;

  flake.modules.darwin.freetube = freetube;
}
