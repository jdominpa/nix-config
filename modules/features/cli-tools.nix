let
  sharedPackages =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        btop
        coreutils
        curl
        devenv
        fd
        fzf
        gawk
        gnupg
        gnused
        gnutar
        just
        man
        man-pages
        neovim
        nix-melt # Ranger-like `flake.lock` file viewer
        nix-tree # Nix dependency tree
        ripgrep
        tree
        unrar
        unzip
        wget
        which
        zip
      ];
      programs.direnv.enable = true;
    };
in
{
  flake.modules.nixos.cli-tools =
    { pkgs, ... }:
    {
      imports = [ sharedPackages ];
      environment.systemPackages = with pkgs; [
        exfat
        hfsprogs
        lm_sensors
        ntfs3g
        pciutils
        usbutils
      ];
    };

  flake.modules.darwin.cli-tools = {
    imports = [ sharedPackages ];
  };
}
