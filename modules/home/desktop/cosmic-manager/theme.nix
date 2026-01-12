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
      wayland.desktopManager.cosmic.appearance = {
        theme = {
          dark = {
            accent = mkRON "optional" {
              red = 0.47;
              green = 0.66;
              blue = 0.99;
            };
            active_hint = 0;
            gaps = mkRON "tuple" [
              0
              0
            ];
          };
          mode = "dark";
        };
        toolkit = {
          apply_theme_global = true;
          header_size = mkRON "enum" "Compact";
          interface_font = {
            family = "Aporetic Sans";
            stretch = mkRON "enum" "Normal";
            style = mkRON "enum" "Normal";
            weight = mkRON "enum" "Normal";
          };
          monospace_font = {
            family = "Aporetic Sans Mono";
            stretch = mkRON "enum" "Normal";
            style = mkRON "enum" "Normal";
            weight = mkRON "enum" "Normal";
          };
        };
      };
      wayland.desktopManager.cosmic.wallpapers = [
        {
          filter_by_theme = true;
          filter_method = mkRON "enum" "Lanczos";
          output = "all";
          rotation_frequency = 600;
          sampling_method = mkRON "enum" "Random";
          scaling_mode = mkRON "enum" "Stretch";
          source = mkRON "enum" {
            value = [
              (mkRON "enum" {
                value = [
                  (mkRON "tuple" [
                    0.23
                    0.23
                    0.23
                  ])
                ];
                variant = "Single";
              })
            ];
            variant = "Color";
          };
        }
      ];
    };
  };
}
