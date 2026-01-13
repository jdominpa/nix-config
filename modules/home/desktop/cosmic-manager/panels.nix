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
      wayland.desktopManager.cosmic.panels = [
        {
          anchor = mkRON "enum" "Top";
          anchor_gap = false;
          autohide = mkRON "optional" null;
          background = mkRON "enum" "ThemeDefault";
          expand_to_edges = true;
          margin = 0;
          name = "Panel";
          opacity = 1.0;
          output = mkRON "enum" "All";
          plugins_center = mkRON "optional" [ "com.system76.CosmicAppletWorkspaces" ];
          plugins_wings = mkRON "optional" (
            mkRON "tuple" [
              [ "com.system76.CosmicAppletPower" ]
              [
                "com.system76.CosmicAppletInputSources"
                "com.system76.CosmicAppletStatusArea"
                "com.system76.CosmicAppletTiling"
                "com.system76.CosmicAppletAudio"
                "com.system76.CosmicAppletNetwork"
                "com.system76.CosmicAppletNotifications"
                "com.system76.CosmicAppletTime"
              ]
            ]
          );
          size = mkRON "enum" "XS";
        }
        {
          anchor = mkRON "enum" "Bottom";
          anchor_gap = true;
          autohide = mkRON "optional" {
            handle_size = 4;
            transition_time = 100;
            wait_time = 50;
          };
          background = mkRON "enum" "ThemeDefault";
          exclusive_zone = false; # see https://github.com/HeitorAugustoLN/cosmic-manager/issues/38#issuecomment-3673900560
          expand_to_edges = false;
          margin = 2;
          name = "Dock";
          opacity = 1.0;
          output = mkRON "enum" "All";
          plugins_center = mkRON "optional" [
            "com.system76.CosmicPanelAppButton"
            "com.system76.CosmicAppList"
            "com.system76.CosmicAppletMinimize"
          ];
          plugins_wings = mkRON "optional" null;
          size = mkRON "enum" "M";
        }
      ];
    };
  };
}
