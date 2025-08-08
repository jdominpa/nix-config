{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.apps.google-drive;
  inherit (config.jdp.darwin.system) homebrew;
in
{
  options.jdp.darwin = {
    apps.google-drive.enable = lib.mkEnableOption "Install Google Drive client for macOS.";
  };

  config = lib.mkIf cfg.enable {
    warnings =
      lib.optional (!homebrew.enable)
        "`jdp.darwin.apps.google-drive` is enabled but `jdp.darwin.system.homebrew` is disabled. Google Drive will not be installed.";

    homebrew.casks = lib.optionals homebrew.enable [
      "google-drive"
    ];
  };
}
