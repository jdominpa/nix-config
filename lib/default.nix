{ lib, ... }:
with lib;
{
  vars = {
    username = "jdominpa";
    userFullName = "Joan Domingo Pasarin";
    userEmail = "jdomingopasarin@icloud.com";
  };

  nixosSystem = import ./nixosSystem.nix;
  darwinSystem = import ./darwinSystem.nix;

  # Use path relative to root of the project
  relativeToRoot = path.append ../.;
  scanPaths =
    path:
    map (f: (path + "/${f}")) (
      attrNames (
        attrsets.filterAttrs
          (
            path: type:
            (type == "directory") # include directories
            || (
              (path != "default.nix") # ignore default.nix
              && (strings.hasSuffix ".nix" path)
            )
          ) # include .nix files

          (builtins.readDir path)
      )
    );
}
