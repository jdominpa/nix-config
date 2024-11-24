{
  config,
  lib,
  ...
}: let
  cfg = config.jdp.home.cli.fzf;
in {
  options.jdp = {
    home.cli.fzf.enable = lib.mkEnableOption "Enable fzf fuzzy finder.";
  };

  config = lib.mkIf cfg.enable {
    programs.fzf.enable = true;
  };
}
