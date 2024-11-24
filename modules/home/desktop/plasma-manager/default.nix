{
  config,
  inputs,
  lib,
  myLib,
  pkgs,
  ...
}: let
  cfg = config.jdp.home.plasma-manager;
in {
  imports =
    (myLib.scanPaths ./.)
    ++ [
      inputs.plasma-manager.homeManagerModules.plasma-manager
    ];

  options.jdp = {
    home.desktop.plasma-manager.enable = lib.mkEnableOption "Whether to enable configurations for KDE Plasma.";
  };

  config = lib.mkIf cfg.enable {
    programs.plasma = {
      enable = true;
      overrideConfig = true;
      immutableByDefault = true;
      krunner.position = "center";
    };

    # Extra KDE applications
    home.packages = with pkgs.kdePackages; [
      kcalc
    ];
  };
}
