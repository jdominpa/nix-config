{
  inputs,
  config,
  lib,
  ...
}: {
  imports = [
    inputs.plasma-manager.homeManagerModules.plasma-manager
    ./files.nix
    ./input.nix
    ./konsole.nix
    ./kscreenlocker.nix
    ./kwin.nix
    ./panels.nix
    ./shortcuts.nix
    ./theme.nix
  ];

  options.plasma-manager.enable = lib.mkEnableOption "Whether to enable configurations for KDE Plasma.";

  config = lib.mkIf config.plasma-manager.enable {
    programs.plasma = {
      enable = true;
      overrideConfig = true;
      immutableByDefault = true;
      krunner.position = "center";
    };
  };
}
