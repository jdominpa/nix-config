{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.desktop.plasma-manager;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    desktop.plasma-manager.input = {
      mice = lib.mkOption {
        type = lib.types.listOf lib.types.attrs;
        default = [ ];
        description = ''
          Mice configurations for plasma-manager. See the documentation of
          plasma-manager for the available options
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
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
          inherit (cfg.input) mice;
        };
      };
    };
  };
}
