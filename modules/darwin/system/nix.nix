{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.nix;
in
{
  options.jdp.darwin = {
    system.nix.enable = mkEnableOption "Enable nix settings specific to macOS.";
  };

  config = mkIf cfg.enable {
    nix = {
      gc = {
        automatic = true;
        interval = [
          {
            Weekday = 1;
            Hour = 10;
            Minute = 0;
          }
        ];
        options = "--delete-older-than 7d";
      };
      optimise = {
        automatic = true;
        interval = [
          {
            Weekday = 1;
            Hour = 10;
            Minute = 0;
          }
        ];
      };
    };
  };
}
