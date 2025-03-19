{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.desktop.plasma-manager;
  inherit (config.jdp.base) user;
in
{
  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.konsole = {
        enable = true;
        defaultProfile = "default";
        customColorSchemes = {
          modus-vivendi = jdp.relativeToRoot "config/konsole/modus-vivendi.colorscheme";
        };
        profiles = {
          default = {
            colorScheme = "modus-vivendi";
            font.size = 13;
          };
        };
      };
    };
  };
}
