{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.homebrew;
in
{
  options.jdp.darwin = {
    system.homebrew.enable = mkEnableOption "Enable managing homebrew with nix-darwin on this system.";
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      onActivation = {
        cleanup = "zap";
      };
    };
  };
}
