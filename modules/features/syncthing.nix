{
  config,
  ...
}:
let
  inherit (config.user) username homeDirectory;
in
{
  flake.modules.nixos.syncthing = {
    services.syncthing = {
      enable = true;
      user = username;
      group = "users";
      configDir = "${homeDirectory.linux}/.config/syncthing";
      dataDir = homeDirectory.linux;
      openDefaultPorts = true;
    };
  };

  flake.modules.darwin.syncthing = {
    homebrew.casks = [ "syncthing-app" ];
  };
}
