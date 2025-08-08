{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.apps.google-chrome;
  inherit (config.jdp.darwin.system) homebrew;
in
{
  options.jdp.darwin = {
    apps.google-chrome.enable = lib.mkEnableOption "Install Google Chrome web browser.";
  };

  config = lib.mkIf cfg.enable {
    warnings =
      lib.optional (!homebrew.enable)
        "`jdp.darwin.apps.google-chrome` is enabled but `jdp.darwin.system.homebrew` is disabled. Google Chrome will not be installed.";

    homebrew.casks = lib.optionals homebrew.enable [
      "google-chrome"
    ];
  };
}
