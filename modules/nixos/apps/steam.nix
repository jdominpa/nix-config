{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.apps.steam;
in
{
  options.jdp.nixos = {
    apps.steam.enable = mkEnableOption "Enable Steam client.";
  };

  config = mkIf cfg.enable {
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
      gamescopeSession.enable = true;
    };

    environment.systemPackages = with pkgs; [
      mangohud # FPS monitoring
      protonup
    ];

    # Game mode
    programs.gamemode.enable = true;
  };
}
