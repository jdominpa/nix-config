{
  config,
  lib,
  myLib,
  ...
}: let
  cfg = config.jdp.home.cli.git;
in {
  options.jdp = {
    home.cli.git.enable = lib.mkEnableOption "Configure git.";
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      userName = myLib.vars.userFullName;
      userEmail = myLib.vars.userEmail;
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
}
