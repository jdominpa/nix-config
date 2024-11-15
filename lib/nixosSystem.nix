{
  inputs,
  system,
  specialArgs,
  nixosModules,
  homeManagerModules ? [],
  myLib,
  ...
}: let
  inherit (inputs) nixpkgs home-manager;
  inherit (nixpkgs) lib;
in
  nixpkgs.lib.nixosSystem {
    inherit system specialArgs;
    modules =
      nixosModules
      ++ (
        lib.optionals ((lib.lists.length homeManagerModules) > 0)
        [
          home-manager.nixosModules.home-manager
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
