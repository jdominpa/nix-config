{
  config,
  lib,
  outputs,
  ...
}:
let
  cfg = config.jdp.base.system.nix;
  inherit (config.jdp.base) user;
in
{
  options.jdp.base = {
    system.nix.enable = lib.mkEnableOption "Enable essential configurations for Nix.";
  };

  config = lib.mkIf cfg.enable {
    nixpkgs = {
      config.allowUnfree = true;
      overlays = builtins.attrValues outputs.overlays;
    };
    nix.settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      trusted-users = [ user.name ];
      substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };
}
