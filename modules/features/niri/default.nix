{
  config,
  ...
}:
let
  wrappers = config.flake.wrappers;
in
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
    { pkgs, wlib, ... }:
    {
      imports = [ wlib.wrapperModules.niri ];
      runtimePkgs = [
        (wrappers.kitty.wrap { inherit pkgs; })
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
        outputs = {
          "DP-1" = {
            mode = "1920x1080@143.855";
            position = _: {
              props = {
                x = 0;
                y = 0;
              };
            };
            scale = 1.0;
            variable-refresh-rate = _: { };
          };
          "DP-2" = {
            focus-at-startup = _: { };
            mode = "2560x1440@240.001";
            position = _: {
              props = {
                x = 1920;
                y = 0;
              };
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
}
