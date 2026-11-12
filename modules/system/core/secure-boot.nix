{
  inputs,
  ...
}:
{
  flake.modules.nixos.secure-boot =
    { lib, pkgs, ... }:
    {
      imports = [ inputs.lanzaboote.nixosModules.lanzaboote ];
      environment.systemPackages = [ pkgs.sbctl ];
      boot = {
        # Lanzaboote replaces the systemd-boot module. NixOS's generated
        # `configuration.nix` sets this to true, so force it off.
        loader.systemd-boot.enable = lib.mkForce false;
        lanzaboote = {
          enable = true;
          pkiBundle = "/var/lib/sbctl";
        };
      };
    };
}
