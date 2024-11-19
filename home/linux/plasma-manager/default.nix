{
  config,
  inputs,
  lib,
  myLib,
  pkgs,
  ...
}: {
  imports =
    (myLib.scanPaths ./.)
    ++ [
      inputs.plasma-manager.homeManagerModules.plasma-manager
    ];

  options.modules = {
    plasma-manager.enable = lib.mkEnableOption "Whether to enable configurations for KDE Plasma.";
  };

  config = lib.mkIf config.modules.plasma-manager.enable {
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
