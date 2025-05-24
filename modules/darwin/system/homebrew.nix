{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.system.homebrew;
in
{
  options.jdp.darwin = {
    system.homebrew.enable = lib.mkEnableOption "Enable managing homebrew with nix-darwin on this system.";
  };

  config = lib.mkIf cfg.enable {
    homebrew = {
      enable = true;
      onActivation = {
        cleanup = "zap";
      };
    };
  };
}
