{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.home.apps.bitwarden;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    apps.bitwarden.enable = mkEnableOption "Install Bitwarden desktop application.";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = [ pkgs.bitwarden-desktop ];
    };
  };
}
