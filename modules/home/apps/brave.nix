{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.jdp.home.apps.brave;
in {
  options.jdp = {
    home.apps.brave.enable = lib.mkEnableOption "Install Brave browser.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [pkgs.brave];
  };
}
