{
  inputs,
  lib,
  ...
}:
{
  imports = [
    inputs.git-hooks.flakeModule
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    { self', ... }:
    {
      treefmt = {
        projectRootFile = "flake.nix";
        programs = {
          deadnix.enable = true;
          nixfmt.enable = true;
          shfmt.enable = true;
          statix.enable = true;
        };
        settings = {
          excludes = [
            "*.el"
            "*.eld"
          ];
          on-unmatched = "warn";
        };
      };

      pre-commit.settings.hooks.nix-fmt = {
        enable = true;
        entry = lib.getExe self'.formatter;
      };
    };
}
