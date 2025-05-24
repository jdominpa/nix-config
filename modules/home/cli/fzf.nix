{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.cli.fzf;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    cli.fzf.enable = lib.mkEnableOption "Enable fzf fuzzy finder.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.fzf.enable = true;
    };
  };
}
