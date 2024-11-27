{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.cli.starship;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    cli.starship.enable = mkEnableOption "Enable starship command line prompt.";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.starship.enable = true;
    };
  };
}
