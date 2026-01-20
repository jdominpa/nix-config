{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.nixos.desktop.cosmic;
in
{
  options.jdp.nixos = {
    desktop.cosmic.enable = lib.mkEnableOption "Enable COSMIC desktop environment.";
  };

  config = lib.mkIf cfg.enable {
    services.desktopManager = {
      cosmic.enable = true;
    };
    environment.systemPackages = [ pkgs.cosmic-ext-calculator ];
  };
}
