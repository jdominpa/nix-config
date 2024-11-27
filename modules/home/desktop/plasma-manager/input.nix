{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.desktop.plasma-manager;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    desktop.plasma-manager.input = {
      mice = mkOption {
        type = types.listOf types.attrs; # TODO: improve the type of this option
        default = [ ];
        description = ''
          Mice configurations for plasma-manager. See the documentation of
          plasma-manager for the available options
        '';
      };
    };
  };

  config = mkIf cfg.enable {
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
          mice = cfg.input.mice;
        };
      };
    };
  };
}
