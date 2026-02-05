{
  flake.modules.darwin.dock = {
    system.defaults.dock = {
      autohide = true; # hide dock since it cannot be disabled
      autohide-delay = 100000.0; # show/hide dock after 100000s (basically never)
      autohide-time-modifier = 0.5; # faster hidding animation
      enable-spring-load-actions-on-all-items = true;
      expose-animation-duration = 0.5; # animation speed of mission control
      expose-group-apps = true; # group windows by application
      launchanim = false; # don't animate opening applications
      mineffect = "scale"; # faster animation when minimizing windows
      minimize-to-application = true; # minimize application to its dock icon
      mru-spaces = false;
      persistent-apps = [ ];
      showhidden = true;
      show-recents = false;
      tilesize = 36;
      wvous-bl-corner = 1;
      wvous-br-corner = 1;
      wvous-tl-corner = 1;
      wvous-tr-corner = 1;
    };
  };
}
