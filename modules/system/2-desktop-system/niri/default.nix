{
  inputs,
  self,
  ...
}:
{
  flake.modules.nixos.niri =
    { pkgs, ... }:
    {
      imports = [ inputs.niri-flake.nixosModules.niri ];
      environment = {
        systemPackages = [ pkgs.xwayland-satellite ];
        variables."NIXOS_OZONE_WL" = "1";
      };
      home-manager.sharedModules = [ self.modules.homeManager.niri ];
      programs.niri = {
        enable = true;
        package = pkgs.niri;
      };
      xdg.portal = {
        config.niri = {
          default = [
            "gtk"
            "gnome"
          ];
          "org.freedesktop.impl.portal.Screencast" = [ "gnome" ];
          "org.freedesktop.impl.portal.Secret" = [ "gnome-keyring" ];
        };
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };
    };

  flake.modules.homeManager.niri = {
    programs.niri.settings = {
      gestures.hot-corners.enable = false;
      hotkey-overlay.skip-at-startup = true;
      overview.zoom = 0.5;
      prefer-no-csd = true;
      screenshot-path = "~/Imatges/Screenshots/%Y%m%dT%H%M%S.png";
    };
  };
}
