{
  config,
  ...
}:
let
  home = config.user.homeDirectory.linux;
in
{
  flake.modules.nixos.user-dirs =
    { pkgs, ... }:
    let
      userDirs = pkgs.writeText "user-dirs.dirs" ''
        XDG_DOCUMENTS_DIR="${home}/Documents"
        XDG_DOWNLOAD_DIR="${home}/Downloads"
        XDG_PICTURES_DIR="${home}/Pictures"
      '';
    in
    {
      systemd.tmpfiles.rules = [
        "L+ ${home}/.config/user-dirs.dirs - - - - ${userDirs}"
      ];
    };
}
