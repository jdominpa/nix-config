{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.cli.fzf;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    cli.fzf.enable = mkEnableOption "Enable fzf fuzzy finder.";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.fzf.enable = true;
    };
  };
}
