{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  cfg = config.jdp.home.apps.bitwarden;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    apps.bitwarden = {
      enable = mkEnableOption "Install Bitwarden desktop application.";
      sshAgent = mkEnableOption "Whether to set the environment variable SSH_AUTH_SOCK for Bitwarden's SSH agent.";
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home = {
        # On darwin we install bitwarden with homebrew
        packages = optionals isLinux [ pkgs.bitwarden-desktop ];
        sessionVariables = mkIf cfg.sshAgent {
          SSH_AUTH_SOCK = "${user.homeDirectory}/.bitwarden-ssh-agent.sock";
        };
      };
    };
  };
}
