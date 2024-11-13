{
  config,
  lib,
  ...
}: let
  cfg = config.plasma-manager;
in {
  config = lib.mkIf cfg.enable {
    programs.plasma = {
      input = {
        keyboard = {
          repeatDelay = 180;
          repeatRate = 33;
          layouts = [
            {
              layout = "us";
              displayName = "us";
            }
            {
              layout = "us";
              variant = "intl";
              displayName = "int";
            }
          ];
        };
      };
    };
  };
}
