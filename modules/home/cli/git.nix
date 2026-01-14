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
        settings = {
          commit.gpgSign = true;
          commit.verbose = true;
          user = {
            name = user.fullName;
            email = user.email;
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
  };
}
