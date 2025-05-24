{
  config,
  inputs,
  lib,
  outputs,
  ...
}:
let
  cfg = user.home-manager;
  inherit (config.jdp.base) user;
in
{
  config = lib.mkIf cfg.enable {
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = {
        inherit
          inputs
          outputs
          ;
      };
      users.${user.name} = {
        # Let home-manager manage itself
        programs.home-manager.enable = true;
        home = {
          username = user.name;
          inherit (user) homeDirectory;
          # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
          stateVersion = "24.05";
        };
      };
    };
  };
}
