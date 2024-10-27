{
  lib,
  ...
}: {
  programs.git = {
    enable = true;
    userName = "Joan Domingo Pasarin";
    userEmail = lib.mkDefault "jdomingopasarin@icloud.com";
    extraConfig = {
      init.defaultBranch = "main";
      commit.verbose = true;
      # Automatically track remote branch
      push.autoSetupRemote = true;
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
}
