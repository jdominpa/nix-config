{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = networking.networkmanager;
  inherit (config.jdp.base) user;
  inherit (config.jdp.nixos.system) networking;
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
      inherit (networking) hostName;
      networkmanager.enable = true;
    };
    users.users.${user.name}.extraGroups = [ "networkmanager" ];
  };
}
