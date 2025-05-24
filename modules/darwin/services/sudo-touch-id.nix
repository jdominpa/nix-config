{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.system.sudoTouchId;
in
{
  options.jdp.darwin = {
    system.sudoTouchId.enable = lib.mkEnableOption "Enable using Touch Id for sudo.";
  };

  config = lib.mkIf cfg.enable {
    security.pam.services.sudo_local.touchIdAuth = true;
  };
}
