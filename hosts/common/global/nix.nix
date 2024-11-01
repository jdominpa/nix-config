{
  inputs,
  lib,
  pkgs,
  ...
}: let
  flakeInputs = lib.filterAttrs (_: lib.isType "flake") inputs;
in {
  nix = {
    settings = {
      auto-optimise-store = lib.mkDefault true;
      experimental-features = "nix-command flakes";
      warn-dirty = false;
    };

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than +3"; # Keep the last 3 generations
    };
  };
}
