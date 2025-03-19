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
        };
      };
    };
  };
}
