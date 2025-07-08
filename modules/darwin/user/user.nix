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
    system.primaryUser = "${cfg.name}";
    users.users.${cfg.name} = {
      isHidden = false;
    };
  };
}
