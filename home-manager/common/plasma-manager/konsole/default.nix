{
  config,
  lib,
  ...
}: let
  cfg = config.plasma-manager;
in {
  config = lib.mkIf cfg.enable {
    programs.konsole = {
      enable = true;
      defaultProfile = "default";
      customColorSchemes = {
        modus-vivendi = ./modus-vivendi.colorscheme;
      };
      profiles = {
        default = {
          colorScheme = "modus-vivendi";
          font.size = 13;
        };
      };
    };
  };
}
