{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.desktop.cosmic-greeter;
in
{
  options.jdp.nixos = {
    desktop.cosmic-greeter.enable = lib.mkEnableOption "Whether to enable COSMIC greeter.";
  };

  config = lib.mkIf cfg.enable {
    services.displayManager = {
      cosmic-greeter.enable = true;
    };
  };
}
