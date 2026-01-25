{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.nixos.desktop.niri;
in
{
  imports = [
    inputs.niri-flake.nixosModules.niri
  ];

  options.jdp.nixos = {
    desktop.niri.enable = lib.mkEnableOption "Whether to enable the Niri window manager.";
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        mpv
        nomacs
        qalculate-gtk
        xwayland-satellite
      ];
      variables."NIXOS_OZONE_WL" = "1";
    };
    programs = {
      niri = {
        enable = true;
        package = pkgs.niri;
      };
      thunar = {
        enable = true;
        plugins = with pkgs; [
          thunar-archive-plugin
          thunar-volman
        ];
      };
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
}
