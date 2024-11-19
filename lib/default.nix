{lib, ...}: {
  vars = {
    username = "jdominpa";
    userFullName = "Joan Domingo Pasarin";
    userEmail = "jdomingopasarin@icloud.com";
  };

  nixosSystem = import ./nixosSystem.nix;
  darwinSystem = import ./darwinSystem.nix;

  # Use path relative to root of the project
  relativeToRoot = lib.path.append ../.;
  scanPaths = path:
    builtins.map
    (f: (path + "/${f}"))
    (builtins.attrNames
      (lib.attrsets.filterAttrs
        (
          path: _type:
            (_type == "directory") # include directories
            || (
              (path != "default.nix") # ignore default.nix
              && (lib.strings.hasSuffix ".nix" path) # include .nix files
            )
        )
        (builtins.readDir path)));
}
