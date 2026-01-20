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
    environment.systemPackages = [
      pkgs.xwayland-satellite
    ];
    programs.niri = {
      enable = true;
      package = pkgs.niri;
    };
  };
}
