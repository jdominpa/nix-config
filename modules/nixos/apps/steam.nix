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
      extraCompatPackages = with pkgs; [
        proton-ge-bin
      ];
      dedicatedServer.openFirewall = true;
      gamescopeSession.enable = true;
      localNetworkGameTransfers.openFirewall = true;
      remotePlay.openFirewall = true;
    };

    environment.systemPackages = with pkgs; [
      mangohud # FPS monitoring
    ];

    # Game mode
    programs.gamemode.enable = true;
  };
}
