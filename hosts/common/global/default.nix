{
  outputs,
  pkgs,
  ...
}: {
  imports = [
    ./locale.nix
    ./nix.nix
    ./openssh.nix
    ./zsh.nix
  ];

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config.allowUnfree = true;
  };

  # Packages installed in every system.
  # To search for packages, run:
  # $ nix search program
  environment.systemPackages = with pkgs; [
    fastfetch
    emacs
    git
    just
    neovim
  ];
}
