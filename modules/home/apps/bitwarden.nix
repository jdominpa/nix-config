{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  cfg = config.jdp.home.apps.bitwarden;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    apps.bitwarden = {
      enable = lib.mkEnableOption "Install Bitwarden desktop application.";
      sshAgent = lib.mkEnableOption "Whether to set the environment variable SSH_AUTH_SOCK for Bitwarden's SSH agent.";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home = {
        # On darwin we install bitwarden with homebrew
        packages = lib.optionals isLinux [ pkgs.bitwarden-desktop ];
        sessionVariables = lib.mkIf cfg.sshAgent {
          SSH_AUTH_SOCK =
            "${user.homeDirectory}/"
            + (
              if isLinux then
                ".bitwarden-ssh-agent.sock"
              else
                "Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock"
            );
        };
      };
    };
  };
}
