{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.home.apps.bitwarden;
  user = config.jdp.base.user;
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
        packages = [ pkgs.bitwarden-desktop ];
        sessionVariables = mkIf cfg.sshAgent {
          SSH_AUTH_SOCK = "${user.homeDirectory}/.bitwarden-ssh-agent.sock";
        };
      };
    };
  };
}
