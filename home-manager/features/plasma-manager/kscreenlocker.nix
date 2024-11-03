{
  config,
  lib,
  ...
}: let
  cfg = config.plasma-manager;
in {
  config = lib.mkIf cfg.enable {
    programs.plasma.kscreenlocker = {
      appearance = {
        alwaysShowClock = true;
        showMediaControls = false;
        inherit (config.programs.plasma.workspace) wallpaperPlainColor;
      };
      autoLock = true;
      lockOnResume = true;
      passwordRequired = true;
      passwordRequiredDelay = 10;
      timeout = 10;
    };
  };
}
