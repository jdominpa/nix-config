{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.base.system.packages;
in
{
  options.jdp.base = {
    system.packages.enable = lib.mkEnableOption "Install useful base packages suitable for any system.";
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        coreutils
        curl
        findutils
        fzf
        emacs30
        gawk
        git
        gnugrep
        gnumake
        gnupg
        gnused
        gnutar
        just
        neovim
        nix-melt # Ranger-like `flake.lock` file viewer
        nix-tree # Nix dependency tree
        ripgrep
        tree
        unrar
        wget
        which
        zip
      ];
      variables = {
        EDITOR = "emacsclient -r";
        VISUAL = "emacsclient -r";
      };
    };
  };
}
