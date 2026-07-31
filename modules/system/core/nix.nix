{
  inputs,
  withSystem,
  ...
}:
let
  sharedSettings = { config, ... }: {
    # Use the configured pkgs from perSystem
    nixpkgs.pkgs = withSystem config.nixpkgs.hostPlatform.system ({ pkgs, ... }: pkgs);
    nix.settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };
in
{
  flake.modules.nixos.nix = {
    imports = [ sharedSettings ];
    nix = {
      settings.auto-optimise-store = true;
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
      };
    };
  };

  flake.modules.darwin.nix = {
    imports = [ sharedSettings ];
    nix = {
      gc = {
        automatic = true;
        interval = [
          {
            Weekday = 1;
            Hour = 10;
            Minute = 0;
          }
        ];
        options = "--delete-older-than 7d";
      };
      optimise = {
        automatic = true;
        interval = [
          {
            Weekday = 1;
            Hour = 10;
            Minute = 0;
          }
        ];
      };
    };
  };

  perSystem =
    { system, ... }:
    let
      nixpkgsConfig = {
        allowUnfree = true;
        # FIXME: temporary fix, remove eventually
        permittedInsecurePackages = [
          "electron-39.8.10"
        ];
      };
      nixpkgsOverlays = [
        inputs.emacs-overlay.overlays.package
        (final: _prev: {
          stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};
        })
      ];
    in
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config = nixpkgsConfig;
        overlays = nixpkgsOverlays;
      };
    };
}
