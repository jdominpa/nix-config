{
  inputs,
  ...
}:
{
  flake.modules.nixos.secure-boot =
    { pkgs, ... }:
    {
      imports = [ inputs.lanzaboote.nixosModules.lanzaboote ];
      environment.systemPackages = [ pkgs.sbctl ];
      boot = {
        lanzaboote = {
          enable = true;
          pkiBundle = "/var/lib/sbctl";
        };
      };
    };
}
