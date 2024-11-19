{
  outputs,
  pkgs,
  myLib,
  ...
}: {
  nixpkgs.overlays = builtins.attrValues outputs.overlays;
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
    trusted-users = [myLib.vars.username];
    substituters = [
      "https://nix-community.cachix.org"
    ];
  };
  environment = {
    systemPackages = with pkgs; [
      curl
      neofetch
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
      stable.iosevka-comfy.comfy
      just
      neovim
      nix-melt
      nix-output-monitor
      nix-tree
      ripgrep
      tldr
      tree
      unrar
      wget
      which
      zip
    ];
    variables.EDITOR = "emacsclient -r";
  };
}
