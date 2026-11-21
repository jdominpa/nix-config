{
  config,
  ...
}:
let
  home = config.user.homeDirectory.linux;
in
{
  flake.modules.nixos.user-dirs =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      options.userDirs.file = lib.mkOption {
        type = lib.types.package;
        readOnly = true;
        default = pkgs.writeText "user-dirs.dirs" ''
          XDG_DOCUMENTS_DIR="${home}/Documents"
          XDG_DOWNLOAD_DIR="${home}/Downloads"
          XDG_PICTURES_DIR="${home}/Pictures"
        '';
        description = ''
          The `user-dirs.dirs` file, linked into the home directory and
          mounted in the nixpak sandboxes.
        '';
      };

      config.systemd.tmpfiles.rules = [
        "L+ ${home}/.config/user-dirs.dirs - - - - ${config.userDirs.file}"
      ];
    };
}
