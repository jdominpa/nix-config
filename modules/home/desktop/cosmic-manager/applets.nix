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
      wayland.desktopManager.cosmic.applets = {
        app-list.settings = {
          enable_drag_source = true;
          favorites = [
            "com.system76.CosmicFiles"
            "google-chrome"
            "com.system76.CosmicTerm"
            "emacs"
            "discord"
            "steam"
          ];
          filter_top_levels = mkRON "optional" null;
        };
        audio.settings.show_media_controls_in_top_panel = true;
        time.settings = {
          first_day_of_week = 0;
          military_time = true;
          show_date_in_top_panel = true;
          show_seconds = false;
          show_weekday = true;
        };
      };
    };
  };
}
