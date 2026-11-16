{
  config,
  ...
}:
{
  flake.modules.nixos.login-manager = {
    services = {
      displayManager.noctalia-greeter.enable = true;
      greetd.settings.initial_session = {
        user = config.user.username;
        command = "niri-session";
      };
    };
  };
}
