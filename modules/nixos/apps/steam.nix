{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.nixos.apps.steam;
in
{
  options.jdp.nixos = {
    apps.steam.enable = lib.mkEnableOption "Enable Steam client.";
  };

  config = lib.mkIf cfg.enable {
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
      gamescopeSession.enable = true;
    };

    environment.systemPackages = with pkgs; [
      mangohud # FPS monitoring
      protonup-ng
    ];

    # Game mode
    programs.gamemode.enable = true;
  };
}
