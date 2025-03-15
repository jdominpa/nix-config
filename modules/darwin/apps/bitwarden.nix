{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.apps.bitwarden;
in
{
  config = mkIf cfg.enable {
    warnings =
      optional (!config.jdp.darwin.system.homebrew.enable)
        "`jdp.home.apps.bitwarden` is enabled but `jdp.darwin.system.homebrew` is disabled. Bitwarden will not be installed.";

    homebrew.masApps = mkIf config.jdp.darwin.system.homebrew.enable {
      Bitwarden = 1352778147;
    };
  };
}
