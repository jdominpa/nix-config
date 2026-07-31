{
  lib,
  moduleLocation,
  ...
}:
{
  # flake-parts declares flake.nixosModules, but nothing declares
  # flake.darwinModules. This is a direct port of nixosModules
  options.flake.darwinModules = lib.mkOption {
    type = lib.types.lazyAttrsOf lib.types.deferredModule;
    default = { };
    apply = lib.mapAttrs (
      name: module: {
        _class = "darwin";
        _file = "${toString moduleLocation}#darwinModules.${name}";
        imports = [ module ];
      }
    );
    description = "nix-darwin modules.";
  };
}
