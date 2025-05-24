{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.system.networking;
in
{
  options.jdp.darwin = {
    system.networking = {
      enable = lib.mkEnableOption "Enable networking settings.";
      hostName = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "Hostname";
        example = "foo";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    networking = {
      inherit (cfg) hostName;
      computerName = cfg.hostName;
    };
    system.defaults.smb.NetBIOSName = cfg.hostName;
  };
}
