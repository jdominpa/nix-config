{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.cli.git;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    cli.git.enable = mkEnableOption "Configure git.";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.git = {
        enable = true;
        userName = user.fullName;
        userEmail = user.email;
        extraConfig = {
          init.defaultBranch = "main";
          user.signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGebTck6crA64QvOnpPVBHgB7nzIX18+FU9nANAaE2W4";
          gpg.format = "ssh";
          commit.gpgSign = false; # FIXME: disabled until bitwarden's ssh agent is fixed on macOS
          commit.verbose = true;
          # Automatically track remote branch
          push.autoSetupRemote = true;
          core.editor = "emacsclient -r";
        };
        aliases = {
          a = "add";
          b = "branch";
          c = "commit";
          f = "fetch";
          l = "log";
          m = "merge";
          p = "push";
          s = "status";
          co = "checkout";
        };
        ignores = [ ".direnv" ];
      };
    };
  };
}
