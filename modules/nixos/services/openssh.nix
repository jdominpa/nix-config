{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.services.openssh;
in
{
  options.jdp.nixos = {
    services.openssh.enable = mkEnableOption "Enable OpenSSH.";
  };

  config = mkIf cfg.enable {
    services.openssh.enable = true;
  };
}
