{
  config,
  lib,
  ...
}:
with lib;
let
  networking = config.jdp.nixos.system.networking;
  cfg = networking.networkmanager;
  user = config.jdp.base.user;
in
{
  options.jdp.nixos = {
    system.networking = {
      hostName = mkOption {
        type = types.str;
        default = "";
        description = "Hostname";
        example = "foo";
      };
      networkmanager.enable = mkEnableOption "Enable NetworkManager.";
    };
  };

  config = mkIf cfg.enable {
    networking = {
      hostName = networking.hostName;
      networkmanager.enable = true;
    };
    users.users.${user.name}.extraGroups = [ "networkmanager" ];
  };
}
