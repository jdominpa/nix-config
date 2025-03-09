{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.home.desktop.plasma-manager;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    desktop.plasma-manager.enable = mkEnableOption "Whether to enable configurations for KDE Plasma.";
  };

  config = mkIf cfg.enable {
    home-manager = {
      sharedModules = [ inputs.plasma-manager.homeManagerModules.plasma-manager ];
      users.${user.name} = {
        programs.plasma = {
          enable = true;
          overrideConfig = true;
          immutableByDefault = true;
        };

        # Extra KDE applications
        home.packages = with pkgs.kdePackages; [
          kcalc
        ];
      };
    };
  };
}
