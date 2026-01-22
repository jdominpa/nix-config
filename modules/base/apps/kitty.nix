{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.base.apps.kitty;
in
{
  options.jdp.base = {
    apps.kitty.enable = lib.mkEnableOption "Install Kitty terminal emulator.";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.kitty ];
  };
}
