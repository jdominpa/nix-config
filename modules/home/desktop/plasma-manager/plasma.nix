{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.home.desktop.plasma-manager;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    desktop.plasma-manager.enable = lib.mkEnableOption "Whether to enable configurations for KDE Plasma.";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.jdp.nixos.desktop.plasma.enable;
        message = "`jdp.nixos.desktop.plasma` needs to be enabled if `jdp.home.desktop.plasma-manager` is enabled.";
      }
    ];

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
