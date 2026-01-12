{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.services.cosmic-greeter;
in
{
  options.jdp.nixos = {
    services.cosmic-greeter.enable = lib.mkEnableOption "Whether to enable COSMIC greeter.";
  };

  config = lib.mkIf cfg.enable {
    services.displayManager = {
      cosmic-greeter.enable = true;
    };
  };
}
