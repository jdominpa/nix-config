{
  config,
  lib,
  ...
}: {
  options = {
    steam.enable = lib.mkOption {
      description = "Enable and configure steam.";
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf config.steam.enable {
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
    };
  };
}
