{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.cli.direnv;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    cli.direnv.enable = mkEnableOption "Enable direnv.";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
      };
    };
  };
}
