{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.home.desktop.niri;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    desktop.niri = {
      enable = lib.mkEnableOption "Whether to enable configurations for the Niri window manager.";
      outputs = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = null;
        description = "Attribute set of outputs for niri.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.jdp.nixos.desktop.niri.enable;
        message = "`jdp.nixos.desktop.niri` needs to be enabled if `jdp.home.desktop.niri` is enabled.";
      }
    ];

    home-manager.users.${user.name} = {
      programs.niri.settings = {
        environment = {
          "NIXOS_OZONE_WL" = "1";
        };
        gestures.hot-corners.enable = false;
        hotkey-overlay.skip-at-startup = true;
        input = {
          keyboard = {
            repeat-delay = 200;
            repeat-rate = 30;
            xkb = {
              layout = "us,us";
              variant = ",intl";
            };
          };
          mouse = {
            accel-profile = "flat";
            accel-speed = 0.2;
          };
          warp-mouse-to-focus.enable = true;
        };
        layout = {
          border = {
            enable = true;
            width = 1;
          };
          gaps = 12;
          shadow.enable = true;
          struts = {
            left = 32;
            right = 32;
          };
        };
        inherit (cfg) outputs;
        overview.zoom = 0.5;
        prefer-no-csd = true;
        screenshot-path = "${user.homeDirectory}/Imatges/Screenshots/%Y%m%dT%H%M%S.png";
      };
    };
  };
}
