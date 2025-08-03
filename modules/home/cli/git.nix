{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.cli.git;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    cli.git.enable = lib.mkEnableOption "Configure git.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      programs.git = {
        enable = true;
        userName = user.fullName;
        userEmail = user.gitEmail;
        extraConfig = {
          init.defaultBranch = "main";
          user.signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGebTck6crA64QvOnpPVBHgB7nzIX18+FU9nANAaE2W4";
          gpg.format = "ssh";
          commit.gpgSign = true;
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
