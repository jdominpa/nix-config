{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.home.apps.brave;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    apps.brave.enable = lib.mkEnableOption "Install Brave browser.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = [ pkgs.brave ];
    };
  };
}
