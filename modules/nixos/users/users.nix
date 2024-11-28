{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.base.user;
in
{
  config = mkIf cfg.enable {
    users.users.${cfg.name} = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };
  };
}
