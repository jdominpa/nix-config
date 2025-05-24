{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.system.nix;
in
{
  options.jdp.nixos = {
    system.nix.enable = lib.mkEnableOption "Enable nix settings specific to NixOS.";
  };

  config = lib.mkIf cfg.enable {
    nix = {
      settings.auto-optimise-store = true;
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
      };
    };
  };
}
