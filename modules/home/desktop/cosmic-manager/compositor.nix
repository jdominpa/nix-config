{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.cosmic-manager;
  inherit (config.jdp.base) user;
  inherit (config.home-manager.users.${user.name}.lib.cosmic) mkRON;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      wayland.desktopManager.cosmic.compositor = {
        active_hint = true;
        autotile = true;
        autotile_behavior = mkRON "enum" "PerWorkspace";
        cursor_follows_focus = true;
        descale_xwayland = false;
        focus_follows_cursor = false;
        input_default = {
          acceleration = mkRON "optional" {
            profile = mkRON "optional" (mkRON "enum" "Flat");
            speed = 0.0;
          };
          state = mkRON "enum" "Enabled";
        };
        workspaces = {
          workspace_layout = mkRON "enum" "Vertical";
          workspace_mode = mkRON "enum" "OutputBound";
        };
        xkb_config = {
          layout = "us,us";
          model = "pc104";
          options = mkRON "optional" "terminate:ctrl_alt_bksp";
          repeat_delay = 200;
          repeat_rate = 30;
          rules = "";
          variant = ",intl";
        };
      };
    };
  };
}
