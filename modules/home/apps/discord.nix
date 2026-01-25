{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.home.apps.discord;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    apps.discord.enable = lib.mkEnableOption "Install Discord.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = if pkgs.stdenv.hostPlatform.isLinux then [ pkgs.vesktop ] else [ pkgs.discord ];
    };
  };
}
