{
  config,
  ...
}:
let
  inherit (config.user) fullName email signingKey;
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
          name = fullName;
          inherit email;
          signingkey = signingKey;
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
