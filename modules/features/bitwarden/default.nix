{
  # NOTE: the ssh auth socket symlink is setup in the zsh module, since
  # otherwise using `environment.sessionVariables` doesn't work because it gets
  # overwritten by the gnome-keyring niri config.

  flake.nixosModules.bitwarden = { pkgs, ... }: {
    environment = {
      systemPackages = [ pkgs.bitwarden-desktop ];
    };
  };

  flake.darwinModules.bitwarden = {
    homebrew.masApps = {
      Bitwarden = 1352778147;
    };
  };
}
