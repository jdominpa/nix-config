{
  config,
  lib,
  ...
}: let
  cfg = config.jdp.home.plasma-manager;
in {
  config = lib.mkIf cfg.enable {
    programs.plasma = {
      fonts = {
        fixedWidth = {
          family = "Iosevka Comfy";
          pointSize = 10;
        };
      };
      workspace = {
        clickItemTo = "select";
        colorScheme = "BreezeDark";
        iconTheme = "Breeze Dark";
        lookAndFeel = "org.kde.breezedark.desktop";
        theme = "breeze-dark";
        wallpaperPlainColor = "000000";
      };
    };
  };
}
