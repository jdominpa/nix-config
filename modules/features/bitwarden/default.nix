{
  flake.nixosModules.bitwarden = { pkgs, ... }: {
    environment = {
      systemPackages = [ pkgs.bitwarden-desktop ];
      variables.SSH_AUTH_SOCK = "/home/jdominpa/.bitwarden-ssh-agent.sock";
    };
  };

  flake.darwinModules.bitwarden = {
    environment.variables.SSH_AUTH_SOCK = "/Users/jdominpa/Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock";
    homebrew.masApps = {
      Bitwarden = 1352778147;
    };
  };
}
