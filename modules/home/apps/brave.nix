{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.home.apps.brave;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    apps.brave.enable = mkEnableOption "Install Brave browser.";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = [ pkgs.brave ];
    };
  };
}
