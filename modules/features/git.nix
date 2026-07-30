{
  inputs,
  moduleWithSystem,
  ...
}:
let
  module = moduleWithSystem (
    { self', ... }: {
      environment.systemPackages = [ self'.packages.git ];
    }
  );
in
{
  flake.nixosModules.git = module;

  flake.darwinModules.git = module;

  perSystem = { pkgs, ... }: {
    packages.git = inputs.wrappers.wrappers.git.wrap {
      inherit pkgs;
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
  };
}
