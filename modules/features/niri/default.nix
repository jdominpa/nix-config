{
  self,
  ...
}:
{
  flake.modules.nixos.niri =
    { config, pkgs, ... }:
    {
      programs.niri = {
        enable = true;
        package = config.wrappers.niri.wrapper;
        useNautilus = false;
      };
      xdg.portal = {
        config.niri."org.freedesktop.impl.portal.Screencast" = [ "gnome" ];
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };
      # Disable gnome-keyring ssh agent since we use bitwarden's ssh agent
      systemd.user.sockets.gcr-ssh-agent.enable = false;
    };

  perSystem =
    { pkgs, ... }:
    {
      wrappers.packages.niri = !pkgs.stdenv.hostPlatform.isLinux;
    };

  flake.wrappers.niri =
    {
      lib,
      pkgs,
      wlib,
      ...
    }:
    let
      noctaliaExe = lib.getExe (self.wrappers.noctalia-shell.wrap { inherit pkgs; });
    in
    {
      imports = [ wlib.wrapperModules.niri ];
      runtimePkgs = [
        (self.wrappers.kitty.wrap { inherit pkgs; })
        pkgs.bibata-cursors
        pkgs.brightnessctl
        pkgs.playerctl
        pkgs.wireplumber # wpctl
        pkgs.xwayland-satellite
      ];
      settings = {
        cursor = [
          { xcursor-theme = "Bibata-Modern-Ice"; }
          { xcursor-size = 24; }
        ];
        gestures.hot-corners.off = _: { };
        hotkey-overlay.skip-at-startup = _: { };
        overview = {
          workspace-shadow.off = _: { }; # needed for overview mode with noctalia
          zoom = 0.5;
        };
        prefer-no-csd = _: { };
        screenshot-path = "~/Imatges/Screenshots/%Y%m%dT%H%M%S.png";
        spawn-at-startup = [ "${noctaliaExe}" ];
      };
    };
}
