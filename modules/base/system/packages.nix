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
        btop
        curl
        exfat # ExFAT drives
        findutils
        fzf
        emacs
        gawk
        git
        gnugrep
        gnumake
        gnupg
        gnused
        gnutar
        hfsprogs # macOS drives
        stable.iosevka-comfy.comfy
        just
        neofetch
        neovim
        nix-melt # Ranger-like `flake.lock` file viewer
        nix-tree # Nix dependency tree
        ntfs3g # Windows drives
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
