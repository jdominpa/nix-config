{
  inputs,
  ...
}:
{
  imports = [
    inputs.git-hooks.flakeModule
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    { config, ... }:
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

      pre-commit.settings.hooks.treefmt.enable = true;
      devShells.default = config.pre-commit.devShell;
    };
}
