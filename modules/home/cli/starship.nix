{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.cli.starship;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    cli.starship.enable = lib.mkEnableOption "Enable starship command line prompt.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.starship.enable = true;
    };
  };
}
