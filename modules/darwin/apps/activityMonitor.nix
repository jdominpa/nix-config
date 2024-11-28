{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.apps.activityMonitor;
in
{
  options.jdp.darwin = {
    apps.activityMonitor.enable = mkEnableOption "Configure Activity Monitor app.";
  };

  config = mkIf cfg.enable {
    system.defaults.ActivityMonitor = {
      IconType = 5;
      ShowCategory = 100;
      SortColumn = "CPUUsage";
      SortDirection = 0;
    };
  };
}
