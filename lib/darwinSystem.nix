{
  inputs,
  system,
  specialArgs,
  darwinModules,
  homeManagerModules ? [],
  myLib,
  ...
}: let
  inherit (inputs) nixpkgs nix-darwin home-manager;
  inherit (nixpkgs) lib;
in
  nix-darwin.lib.darwinSystem {
    inherit system specialArgs;
    modules =
      darwinModules
      ++ (
        lib.optionals ((lib.lists.length homeManagerModules) > 0)
        [
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = specialArgs;
              users."${myLib.vars.username}".imports = homeManagerModules;
            };
          }
        ]
      );
  }
