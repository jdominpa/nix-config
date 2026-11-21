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
          # Hide user files from Steam, Proton and games (Wine maps `/` to
          # `Z:`). This isn't a complete sandbox (see WA-005).
          package = pkgs.steam.override {
            extraPreBwrapCmds = ''
              mkdir -p "$HOME/.local/share/steam-home"
            '';
            extraBwrapArgs = [
              # Steam's own directory replaces the home directory
              ''--bind "$HOME/.local/share/steam-home" "$HOME"''
              # btrbk snapshots and copies of /home
              "--tmpfs /.snapshots"
              "--tmpfs /data/backup"
              # MangoHud reads its configuration from the real home
              ''--ro-bind-try "$HOME/.config/MangoHud" "$HOME/.config/MangoHud"''
            ];
          };
          extraCompatPackages = [ pkgs.proton-ge-bin ];
          gamescopeSession.enable = true;
          # The firewall blocks Remote Play otherwise
          remotePlay.openFirewall = true;
        };
      };
    };
}
