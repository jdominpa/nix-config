{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.base.user;
in
{
  config = lib.mkIf cfg.enable {
    users.users.${cfg.name} = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };
  };
}
