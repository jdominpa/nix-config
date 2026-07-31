let
  install = {
    wrappers.git.enable = true;
  };
in
{
  flake.wrappers.git =
    { wlib, ... }:
    {
      imports = [ wlib.wrapperModules.git ];
      settings = {
        commit.gpgSign = true;
        commit.verbose = true;
        user = {
          name = "Joan Domingo Pasarin";
          email = "work@jdompas.com";
          signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGebTck6crA64QvOnpPVBHgB7nzIX18+FU9nANAaE2W4";
        };
        init.defaultBranch = "main";
        gpg.format = "ssh";
        # Automatically track remote branch
        push.autoSetupRemote = true;
        core.editor = "emacsclient -r";
        # GIT_CONFIG_GLOBAL replaces ~/.gitconfig entirely and it is read-only.
        # This keeps an option for per-machine settings
        include.path = "~/.config/git/config.local";
      };
    };

  flake.modules.nixos.git = install;

  flake.modules.darwin.git = install;
}
