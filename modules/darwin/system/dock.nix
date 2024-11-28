{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.dock;
in
{
  options.jdp.darwin = {
    system.dock.enable = mkEnableOption "Enable dock settings.";
  };

  config = mkIf cfg.enable {
    system.defaults.dock = {
      tilesize = 36;
      autohide = true;
      showhidden = true;
      show-recents = false;
      mru-spaces = false;
      expose-group-by-app = true; # group windows by application
      mineffect = "scale"; # faster animation when minimizing windows
      minimize-to-application = true; # minimize application to its dock icon
      enable-spring-load-actions-on-all-items = true;
      launchanim = false; # don't animate opening applications
      expose-animation-duration = 0.5;
    };
  };
}
