{
  config,
  inputs,
  ...
}:
{
  imports = [
    inputs.flake-parts.flakeModules.modules
    inputs.nix-darwin.flakeModules.default
    inputs.wrappers.flakeModules.wrappers
  ];

  systems = [
    "aarch64-darwin"
    "x86_64-linux"
  ];

  flake.modules =
    let
      installWrapperModules = builtins.mapAttrs (_: v: v.install) config.flake.wrappers;
    in
    {
      nixos = installWrapperModules;
      darwin = installWrapperModules;
      generic = installWrapperModules;
    };
}
