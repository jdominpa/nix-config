{
  inputs,
  ...
}:
{
  flake.modules.nixos.nix-index = {
    imports = [ inputs.nix-index-database.nixosModules.nix-index ];
    programs.nix-index-database.comma.enable = true;
  };

  flake.modules.darwin.nix-index = {
    imports = [ inputs.nix-index-database.darwinModules.nix-index ];
    programs.nix-index-database.comma.enable = true;
  };
}
