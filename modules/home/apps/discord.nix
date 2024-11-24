{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.jdp.home.apps.discord;
in {
  options.jdp = {
    home.apps.discord.enable = lib.mkEnableOption "Install Discord.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [pkgs.discord];
  };
}
