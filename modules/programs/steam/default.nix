{
  flake.modules.nixos.steam =
    { pkgs, ... }:
    {
      programs.steam = {
        enable = true;
        extraCompatPackages = [ pkgs.proton-ge-bin ];
        dedicatedServer.openFirewall = true;
        gamescopeSession.enable = true;
        localNetworkGameTransfers.openFirewall = true;
        remotePlay.openFirewall = true;
      };

      environment.systemPackages = [
        pkgs.mangohud # FPS monitoring
      ];

      # Game mode
      programs.gamemode.enable = true;
    };
}
