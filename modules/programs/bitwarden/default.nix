{
  flake.modules.homeManager.bitwarden =
    { config, pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isLinux;
    in
    {
      home = {
        packages = [ pkgs.bitwarden-desktop ];
        sessionVariables.SSH_AUTH_SOCK =
          "${config.home.homeDirectory}/"
          + (
            if isLinux then
              ".bitwarden-ssh-agent.sock"
            else
              "Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock"
          );
      };
    };
}
