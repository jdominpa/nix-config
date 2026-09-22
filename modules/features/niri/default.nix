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
      # FIXME: Steam popup dialogs close instantly on 0.8.2. Cherry-pick the
      # upstream fix, which landed after the tag was cut, until nixpkgs ships
      # it. Mirrors the pending nixpkgs PR, so this becomes a no-op once that
      # merges and can simply be dropped.
      # Upstream: https://github.com/Supreeeme/xwayland-satellite/pull/494
      # Nixpkgs:  https://github.com/NixOS/nixpkgs/pull/564273
      xwaylandSatellite = pkgs.xwayland-satellite.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [
          (pkgs.fetchpatch2 {
            name = "fix-dropdowns-closing-instantly";
            url = "https://github.com/Supreeeme/xwayland-satellite/commit/add2795134593faafce60e404a0a75df68e9ee0c.diff?full_index=1";
            hash = "sha256-6QOZsE4/OoYjzNlzkzMmx6d9rcuh66AHjTBUqHnAWxU=";
          })
        ];
      });
    in
    {
      imports = [ wlib.wrapperModules.niri ];
      runtimePkgs = [
        (self.wrappers.kitty.wrap { inherit pkgs; })
        pkgs.bibata-cursors
        pkgs.brightnessctl
        pkgs.playerctl
        pkgs.wireplumber # wpctl
        xwaylandSatellite
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
