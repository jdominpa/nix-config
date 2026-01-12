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
      programs.cosmic-files = {
        enable = true;
        settings = {
          app_theme = mkRON "enum" "System";
          desktop = {
            show_content = false;
            show_mounted_drives = false;
            show_trash = false;
          };
          favorites = [
            (mkRON "enum" "Home")
            (mkRON "enum" "Documents")
            (mkRON "enum" "Downloads")
            (mkRON "enum" "Music")
            (mkRON "enum" "Pictures")
            (mkRON "enum" "Videos")
          ];
          show_details = true;
          tab = {
            folders_first = true;
            icon_sizes = {
              grid = 100;
              list = 100;
            };
            show_hidden = false;
            view = mkRON "enum" "List";
          };
        };
      };
    };
  };
}
