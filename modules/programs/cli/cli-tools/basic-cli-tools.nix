{
  self,
  ...
}:
let
  basicPackages =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        bat
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
      programs.zoxide.enable = true;
    };

  flake.modules.darwin.basic-cli-tools =
    { lib, pkgs, ... }:
    {
      imports = [ basicPackages ];
      environment.systemPackages = [
        pkgs.zoxide
      ];
      programs.zsh.shellInit = ''
        eval "$(${lib.meta.getExe pkgs.zoxide} init zsh)"
      '';
    };

  flake.modules.homeManager.basic-cli-tools = {
    programs = {
      btop.enable = true;
      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };
      fzf = {
        enable = true;
        enableZshIntegration = true;
      };
    };
  };
}
