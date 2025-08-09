{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.system.sudo-touch-id;
in
{
  options.jdp.darwin = {
    system.sudo-touch-id.enable = lib.mkEnableOption "Enable Touch Id for sudo.";
  };

  config = lib.mkIf cfg.enable {
    security.pam.services.sudo_local.touchIdAuth = true;
  };
}
