{
  config,
  lib,
  myLib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.cli.git;
  user = config.jdp.base.user;
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
      };
    };
  };
}
