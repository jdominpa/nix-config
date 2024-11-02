{
  config,
  lib,
  ...
}: let
  cfg = config.plasma-manager;
in {
  config = lib.mkIf cfg.enable {
    programs.konsole = {
      enable = true;
      defaultProfile = "default";
      profiles = {
        default = {
          font.size = 13;
        };
      };
    };
  };
}
