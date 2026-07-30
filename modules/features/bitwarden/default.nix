{
  flake.modules.darwin.bitwarden = {
    homebrew.masApps = {
      Bitwarden = 1352778147;
    };
  };

  flake.modules.homeManager.bitwarden =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) isLinux;
    in
    {
      home = {
        packages = lib.optionals isLinux [ pkgs.bitwarden-desktop ];
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
