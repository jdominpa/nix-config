{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.keyboard;
in
{
  options.jdp.darwin = {
    system.keyboard.enable = mkEnableOption "Enable keyboard settings.";
  };

  config = mkIf cfg.enable {
    system = {
      defaults.NSGlobalDomain = {
        AppleKeyboardUIMode = 3; # Mode 3 enables full keyboard control.
        ApplePressAndHoldEnabled = false; # disable press and hold
        InitialKeyRepeat = 12; # normal minimum is 15 (225 ms), maximum is 120 (1800 ms)
        KeyRepeat = 2; # normal minimum is 2 (30 ms), maximum is 120 (1800 ms)
      };
      keyboard = {
        enableKeyMapping = true;
        remapCapsLockToEscape = true;
      };
    };
  };
}
