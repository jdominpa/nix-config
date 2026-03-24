{
  flake.modules.darwin.syncthing = {
    homebrew.casks = [ "syncthing-app" ];
  };

  flake.modules.homeManager.syncthing =
    { pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isLinux;
    in
    {
      services.syncthing = {
        enable = true;
        tray.enable = isLinux;
      };
    };
}
