{
  inputs,
  ...
}:
{
  # Not part of `core`: a host can only import this once its signing keys exist
  # in /var/lib/sbctl, see "Enable Secure Boot" in the README.
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
