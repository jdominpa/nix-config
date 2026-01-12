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
      programs.cosmic-term = {
        enable = true;
        colorSchemes = [
          {
            bright = {
              black = "#595959";
              blue = "#79A8FF";
              cyan = "#6AE4B9";
              green = "#70B900";
              magenta = "#B6A0FF";
              red = "#EF8B50";
              white = "#FFFFFF";
              yellow = "#C0C530";
            };
            bright_foreground = "#FFFFFF";
            cursor = "#FFFFFF";
            dim = {
              black = "#000000";
              blue = "#2FAFFF";
              cyan = "#00D3D0";
              green = "#44BC44";
              magenta = "#FEACD0";
              red = "#FF8059";
              white = "#BFBFBF";
              yellow = "#D0BC00";
            };
            dim_foreground = "#FFFFFF";
            foreground = "#FFFFFF";
            mode = "dark";
            name = "Modus Vivendi";
            normal = {
              black = "#000000";
              blue = "#2FAFFF";
              cyan = "#00D3D0";
              green = "#44BC44";
              magenta = "#FEACD0";
              red = "#FF8059";
              white = "#BFBFBF";
              yellow = "#D0BC00";
            };
          }
        ];
        profiles = [
          {
            hold = false;
            is_default = true;
            name = "Modus Vivendi";
            syntax_theme_dark = "Modus Vivendi";
            syntax_theme_light = "COSMIC Light";
          }
        ];
        settings = {
          app_theme = mkRON "enum" "System";
          font_name = "Aporetic Sans Mono";
          font_size = 15;
        };
      };
    };
  };
}
