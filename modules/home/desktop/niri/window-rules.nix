{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.niri;
  inherit (config.jdp.base) user;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.niri.settings = {
        window-rules = [
          {
            matches = [ { app-id = "^emacs(client)?$"; } ];
            open-maximized = true;
          }
          {
            matches = [ { app-id = "^bitwarden$"; } ];
            block-out-from = "screencast";
          }
        ];
        layer-rules = [
          {
            matches = [ { namespace = "^swaync-notification-window$"; } ];
            block-out-from = "screencast";
          }
        ];
      };
    };
  };
}
