{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.base.system.packages;
in
{
  options.jdp.base = {
    system.packages.enable = mkEnableOption "Install useful base packages suitable for any system.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      coreutils
      curl
      findutils
      fzf
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
  };
}
