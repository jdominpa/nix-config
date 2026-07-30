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
          gamescopeSession.enable = true;
        };
      };
    };
}
