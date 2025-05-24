{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.plasma-manager;
  inherit (config.jdp.base) user;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.plasma = {
        krunner = {
          position = "center";
          activateWhenTypingOnDesktop = true;
          shortcuts.launch = "Meta+Space";
        };
      };
    };
  };
}
