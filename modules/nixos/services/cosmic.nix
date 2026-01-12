{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.services.cosmic;
in
{
  options.jdp.nixos = {
    services.cosmic.enable = lib.mkEnableOption "Enable COSMIC desktop environment.";
  };

  config = lib.mkIf cfg.enable {
    services.desktopManager = {
      cosmic.enable = true;
    };
  };
}
