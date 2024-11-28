{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.networking;
in
{
  options.jdp.darwin = {
    system.networking = {
      enable = mkEnableOption "Enable networking settings.";
      hostName = mkOption {
        type = types.str;
        default = "";
        description = "Hostname";
        example = "foo";
      };
    };
  };

  config = mkIf cfg.enable {
    networking = {
      hostName = cfg.hostName;
      computerName = cfg.hostName;
    };
    system.defaults.smb.NetBIOSName = cfg.hostName;
  };
}
