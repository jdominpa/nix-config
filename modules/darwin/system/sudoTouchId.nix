{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.sudoTouchId;
in
{
  options.jdp.darwin = {
    system.sudoTouchId.enable = mkEnableOption "Enable using Touch Id for sudo.";
  };

  config = mkIf cfg.enable {
    security.pam.enableSudoTouchIdAuth = true;
  };
}
