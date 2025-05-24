{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.system.trackpad;
in
{
  options.jdp.darwin = {
    system.trackpad.enable = lib.mkEnableOption "Enable trackpad settings.";
  };

  config = lib.mkIf cfg.enable {
    system.defaults.trackpad = {
      Clicking = true; # enable tap to click
      TrackpadRightClick = true; # enable two finger right click
    };
  };
}
