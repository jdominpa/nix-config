{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  inherit (pkgs) stdenv;
  cfg = config.jdp.home.apps.bitwarden;
  user = config.jdp.base.user;
  homebrew = config.jdp.darwin.system.homebrew;
in
{
  options.jdp.home = {
    apps.bitwarden = {
      enable = mkEnableOption "Install Bitwarden desktop application.";
      sshAgent = mkEnableOption "Whether to set the environment variable SSH_AUTH_SOCK for Bitwarden's SSH agent.";
    };
  };

  config = mkIf cfg.enable {
    warnings =
      optional (stdenv.isDarwin && !homebrew.enable)
        "`jdp.home.apps.bitwarden` is enabled but `jdp.darwin.system.homebrew` is disabled. Bitwarden will not be installed.";

    home-manager.users.${user.name} = {
      home = {
        packages = optionals stdenv.isLinux [ pkgs.bitwarden-desktop ];
        sessionVariables = mkIf cfg.sshAgent {
          SSH_AUTH_SOCK = "${user.homeDirectory}/.bitwarden-ssh-agent.sock";
        };
      };
    };

    homebrew.masApps = mkIf (stdenv.isDarwin && homebrew.enable) {
      Bitwarden = 1352778147;
    };
  };
}
