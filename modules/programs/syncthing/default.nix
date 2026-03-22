{
  flake.modules.homeManager.syncthing =
    { lib, pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isLinux isDarwin;
    in
    {
      home.packages = lib.optionals isDarwin [ pkgs.syncthing-macos ];
      services.syncthing = {
        enable = true;
        tray.enable = isLinux;
      };
    };
}
