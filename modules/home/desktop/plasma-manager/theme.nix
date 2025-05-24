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
          general = {
            family = "Aporetic Sans";
            pointSize = 11;
          };
          fixedWidth = {
            family = "Monospace";
            fixedPitch = true;
            pointSize = 11;
          };
          menu = {
            family = "Aporetic Sans";
            pointSize = 10;
          };
          small = {
            family = "Aporetic Sans";
            pointSize = 8;
          };
          toolbar = {
            family = "Aporetic Sans";
            pointSize = 10;
          };
          windowTitle = {
            family = "Aporetic Sans";
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
