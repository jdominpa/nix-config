{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.system.nix;
in
{
  options.jdp.nixos = {
    system.nix.enable = mkEnableOption "Enable nix settings specific to NixOS.";
  };

  config = mkIf cfg.enable {
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
