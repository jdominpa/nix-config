{
  config,
  inputs,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.cosmic-manager;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    desktop.cosmic-manager.enable = lib.mkEnableOption "Whether to enable configurations for COSMIC.";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.jdp.nixos.services.cosmic.enable;
        message = "`jdp.nixos.services.cosmic` needs to be enabled if `jdp.home.desktop.cosmic-manager` is enabled.";
      }
    ];

    home-manager = {
      sharedModules = [ inputs.cosmic-manager.homeManagerModules.cosmic-manager ];
      users.${user.name} = {
        wayland.desktopManager.cosmic = {
          enable = true;
          resetFiles = true;
        };
      };
    };
  };
}
