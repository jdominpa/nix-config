{
  flake.modules.nixos.gaming =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.mangohud # FPS monitoring
      ];
      programs = {
        coolercontrol.enable = true;
        gamemode.enable = true;
        steam = {
          enable = true;
          extraCompatPackages = [ pkgs.proton-ge-bin ];
          dedicatedServer.openFirewall = true;
          gamescopeSession.enable = true;
          localNetworkGameTransfers.openFirewall = true;
          remotePlay.openFirewall = true;
        };
      };
    };
}
