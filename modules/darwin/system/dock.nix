{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.system.dock;
in
{
  options.jdp.darwin = {
    system.dock.enable = lib.mkEnableOption "Enable dock settings.";
  };

  config = lib.mkIf cfg.enable {
    system.defaults.dock = {
      autohide = true;
      autohide-delay = 0.1; # lower delay before showing/hidding the dock
      autohide-time-modifier = 0.5; # faster hidding animation
      enable-spring-load-actions-on-all-items = true;
      expose-animation-duration = 0.5; # animation speed of mission control
      expose-group-apps = true; # group windows by application
      launchanim = false; # don't animate opening applications
      mineffect = "scale"; # faster animation when minimizing windows
      minimize-to-application = true; # minimize application to its dock icon
      mru-spaces = false;
      persistent-apps = [
        {
          app = "/Applications/Google Chrome.app";
        }
        {
          app = "/System/Applications/Utilities/Terminal.app";
        }
        {
          app = "/Applications/Nix Apps/Emacs.app";
        }
        {
          app = "/Applications/WhatsApp.app";
        }
        {
          app = "/System/Applications/Mail.app";
        }
        {
          app = "/System/Applications/App Store.app";
        }
        {
          app = "/System/Applications/System Settings.app";
        }
      ];
      showhidden = true;
      show-recents = false;
      tilesize = 50;
      wvous-bl-corner = 1;
      wvous-br-corner = 1;
      wvous-tl-corner = 1;
      wvous-tr-corner = 1;
    };
  };
}
