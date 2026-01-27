let
  genericPackages =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        btop
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
in
{
  flake.modules.nixos.cli-tools =
    { pkgs, ... }:
    {
      imports = [ genericPackages ];

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
    imports = [ genericPackages ];
  };
}
