{
  flake.modules.homeManager.git = {
    programs.git = {
      enable = true;
      settings = {
        commit.gpgSign = true;
        commit.verbose = true;
        user = {
          name = "Joan Domingo Pasarin";
          email = "jdomingopasarin@gmail.com";
          signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGebTck6crA64QvOnpPVBHgB7nzIX18+FU9nANAaE2W4";
        };
        init.defaultBranch = "main";
        gpg.format = "ssh";
        # Automatically track remote branch
        push.autoSetupRemote = true;
        core.editor = "emacsclient -r";
      };
      ignores = [ ".direnv" ];
    };
  };
}
