# This file should be included when using standalone home-manager
{
  inputs,
  outputs,
  lib,
  pkgs,
  ...
}: let
  flakeInputs = lib.filterAttrs (_: lib.isType "flake") inputs;
in {
  nix = {
    # package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      warn-dirty = false;
    };
    registry = lib.mapAttrs (_: flake: {inherit flake;}) flakeInputs;
  };

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config.allowUnfree = true;
  };
}
