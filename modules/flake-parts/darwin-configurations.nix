{
  inputs,
  ...
}:
let
  inherit (inputs.nixpkgs) lib;
in
{
  options.flake = inputs.flake-parts.lib.mkSubmoduleOptions {
    darwinConfigurations = lib.mkOption {
      type = lib.types.lazyAttrsOf lib.types.raw;
      default = { };
    };
  };
}
