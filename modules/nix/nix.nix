{
  inputs,
  ...
}:
let
  sharedSettings = {
    nixpkgs = {
      config.allowUnfree = true;
      overlays = [
        # TODO: move these to their respective configurations
        # inputs.niri-flake.overlays.niri
        (final: _prev: {
          stable = inputs.nixpkgs-stable.legacyPackages.${final.system};
        })
      ];
    };
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
}
