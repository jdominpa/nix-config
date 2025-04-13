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
        defaultProfile = "modus-vivendi";
        customColorSchemes = {
          modus-vivendi = jdp.relativeToRoot "config/konsole/modus-vivendi.colorscheme";
        };
        profiles = {
          modus-vivendi = {
            colorScheme = "modus-vivendi";
            font = {
              name = "Monospace";
              size = 13;
            };
          };
        };
      };
    };
  };
}
