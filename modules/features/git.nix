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
        # Check every object received by fetch, clone and push, so malformed
        # objects (an attack vector against git itself) are refused. For an old
        # repository with broken history use `git -c transfer.fsckObjects=false
        # clone ...`.
        transfer.fsckObjects = true;
        core.editor = "emacsclient -r";
        # GIT_CONFIG_GLOBAL replaces ~/.gitconfig entirely and it is read-only.
        # This keeps an option for per-machine settings
        include.path = "~/.config/git/config.local";
      };
    };

  flake.modules.nixos.git = install;

  flake.modules.darwin.git = install;
}
