{
  inputs,
  moduleWithSystem,
  ...
}:
{
  flake.nixosModules.niri = moduleWithSystem (
    { pkgs, self', ... }: {
      programs.niri = {
        enable = true;
        package = self'.packages.niri;
        useNautilus = false;
      };
      xdg.portal = {
        config.niri."org.freedesktop.impl.portal.Screencast" = [ "gnome" ];
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };
      # Disable gnome-keyring ssh agent since we use bitwarden's ssh agent
      systemd.user.sockets.gcr-ssh-agent.enable = false;
    }
  );

  perSystem = { pkgs, self', ... }: {
    packages.niri = inputs.wrappers.wrappers.niri.wrap {
      inherit pkgs;
      imports = [
        ./_binds.nix
        ./_input.nix
        ./_layout.nix
        ./_rules.nix
      ];
      runtimePkgs = [
        self'.packages.kitty
        pkgs.brightnessctl
        pkgs.playerctl
        pkgs.wireplumber # wpctl
        pkgs.xwayland-satellite
      ];
      settings = {
        gestures.hot-corners.off = _: { };
        hotkey-overlay.skip-at-startup = _: { };
        outputs = {
          "DP-1" = {
            mode = "1920x1080@143.855";
            position = _: {
              props = { x = 0; y = 0; };
            };
            scale = 1.0;
            variable-refresh-rate = _: { };
          };
          "DP-2" = {
            focus-at-startup = _: { };
            mode = "2560x1440@240.001";
            position = _: {
              props = { x = 1920; y = 0; };
            };
            scale = 1.0;
            variable-refresh-rate = _: { };
          };
        };
        overview = {
          workspace-shadow.off = _: { }; # needed for overview mode with noctalia
          zoom = 0.5;
        };
        prefer-no-csd = _: { };
        screenshot-path = "~/Imatges/Screenshots/%Y%m%dT%H%M%S.png";
        spawn-at-startup = [ "noctalia" ];
      };
    };
  };
}
