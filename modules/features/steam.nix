{
  flake.modules.nixos.steam =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.mangohud # FPS monitoring
      ];
      programs = {
        gamemode.enable = true;
        steam = {
          enable = true;
          extraCompatPackages = [ pkgs.proton-ge-bin ];
          gamescopeSession.enable = true;
          # The firewall blocks Remote Play otherwise
          remotePlay.openFirewall = true;
        };
      };
    };
}
