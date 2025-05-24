{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.services.openssh;
in
{
  options.jdp.nixos = {
    services.openssh.enable = lib.mkEnableOption "Enable OpenSSH.";
  };

  config = lib.mkIf cfg.enable {
    services.openssh.enable = true;
  };
}
