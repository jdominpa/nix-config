{
  config,
  lib,
  ...
}:
let
  cfg = networking.networkmanager;
  inherit (config.jdp.base) user;
  inherit (config.jdp.nixos.system) networking;
in
{
  options.jdp.nixos = {
    system.networking = {
      hostName = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "Hostname";
        example = "foo";
      };
      networkmanager.enable = lib.mkEnableOption "Enable NetworkManager.";
    };
  };

  config = lib.mkIf cfg.enable {
    networking = {
      inherit (networking) hostName;
      networkmanager.enable = true;
    };
    users.users.${user.name}.extraGroups = [ "networkmanager" ];
  };
}
