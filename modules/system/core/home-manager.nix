{
  inputs,
  ...
}:
let
  sharedSettings = {
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
    };
  };
in
{
  flake.nixosModules.home-manager = {
    imports = [
      inputs.home-manager.nixosModules.home-manager
      sharedSettings
    ];
  };

  flake.darwinModules.home-manager = {
    imports = [
      inputs.home-manager.darwinModules.home-manager
      sharedSettings
    ];
  };
}
