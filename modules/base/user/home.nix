{
  config,
  inputs,
  lib,
  outputs,
  ...
}:
with lib;
let
  user = config.jdp.base.user;
  cfg = user.home-manager;
in
{
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];

  config = mkIf cfg.enable {
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = {
        inherit
          inputs
          outputs
          user
          ;
      };
      users.${user.name}.imports = [
        (
          { user, ... }:
          {
            # Let home-manager manage itself
            programs.home-manager.enable = true;
            home = {
              username = user.name;
              homeDirectory = user.homeDirectory;
              # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
              stateVersion = "24.05";
            };
          }
        )
      ];
    };
  };
}
