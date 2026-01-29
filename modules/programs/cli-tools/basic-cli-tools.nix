{
  self,
  ...
}:
let
  basicPackages =
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
      home-manager.sharedModules = [ self.modules.homeManager.basic-cli-tools ];
    };
in
{
  flake.modules.nixos.basic-cli-tools =
    { pkgs, ... }:
    {
      imports = [ basicPackages ];

      environment.systemPackages = with pkgs; [
        exfat
        hfsprogs
        lm_sensors
        ntfs3g
        pciutils
        usbutils
      ];
    };

  flake.modules.darwin.basic-cli-tools = {
    imports = [ basicPackages ];
  };

  flake.modules.homeManager.basic-cli-tools = {
    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };
  };
}
