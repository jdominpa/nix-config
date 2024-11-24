{
  config,
  lib,
  ...
}: let
  cfg = config.jdp.home.cli.starship;
in {
  options.jdp = {
    home.cli.starship.enable = lib.mkEnableOption "Enable starship command line prompt.";
  };

  config = lib.mkIf cfg.enable {
    programs.starship.enable = true;
  };
}
