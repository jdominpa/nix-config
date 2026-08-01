{
  config,
  ...
}:
let
  inherit (config.user) homeDirectory;
in
{
  flake.modules.nixos.bitwarden = { pkgs, ... }: {
    environment = {
      systemPackages = [ pkgs.bitwarden-desktop ];
      sessionVariables.SSH_AUTH_SOCK = "${homeDirectory.linux}/.bitwarden-ssh-agent.sock";
    };
  };

  flake.modules.darwin.bitwarden = {
    homebrew.masApps = {
      Bitwarden = 1352778147;
    };
    environment.sessionVariables.SSH_AUTH_SOCK = "${homeDirectory.darwin}/Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock";
  };
}
