{
  flake.modules.nixos.login-manager =
    { pkgs, ... }:
    {
      services.greetd = {
        enable = true;
        settings = {
          default_session = {
            command = ''
            ${pkgs.tuigreet}/bin/tuigreet \
              --time \
              --asterisks \
              --remember \
              --remember-session \
              --user-menu
            '';
            user = "greeter";
          };
        };
      };
    };
}
