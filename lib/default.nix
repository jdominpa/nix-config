{lib, ...}: {
  vars = {
    username = "jdominpa";
  };

  nixosSystem = import ./nixosSystem.nix;
  darwinSystem = import ./darwinSystem.nix;

  # Use path relative to root of the project
  relativeToRoot = lib.path.append ../.;
}
