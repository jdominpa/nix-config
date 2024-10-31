{
  config,
  lib,
  ...
}: {
  options = {
    steam.enable = lib.mkEnableOption "Install and configure Steam.";
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
