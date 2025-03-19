{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.home.apps.discord;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    apps.discord.enable = mkEnableOption "Install Discord.";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = [ pkgs.discord ];
    };
  };
}
