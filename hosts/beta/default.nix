{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  hostName = "beta";
  user = config.jdp.base.user;
in
{
  imports = [
    (jdp.relativeToRoot "modules/base")
    (jdp.relativeToRoot "modules/darwin")
    (jdp.relativeToRoot "modules/home")
  ];

  jdp = {
    base = {
      system = {
        nix.enable = true;
        packages.enable = true;
        shell.aliases.enable = true;
      };
      user = {
        enable = true;
        name = "jdominpa";
        fullName = "Joan Domingo Pasarin";
        email = "jdomingopasarin@icloud.com";
        shell = pkgs.zsh;
        homeDirectory = "/Users/${user.name}";
        home-manager.enable = true;
      };
    };
    darwin = {
      apps = {
        activityMonitor.enable = true;
        finder.enable = true;
      };
      system = {
        dock.enable = true;
        fonts.enable = true;
        keyboard.enable = true;
        locale.enable = true;
        networking = {
          enable = true;
          inherit hostName;
        };
        nix.enable = true;
        power.enable = true;
        settings.enable = true;
        shell.zsh.enable = true;
        sudoTouchId.enable = true;
        trackpad.enable = true;
      };
    };
    home = {
      apps = {
        bitwarden = {
          enable = true;
          sshAgent = true;
        };
      };
      cli = {
        direnv.enable = true;
        fzf.enable = true;
        git.enable = true;
        starship.enable = true;
        zsh.enable = true;
      };
      editors.emacs = {
        enable = true;
        withLsp = true;
      };
    };
  };
}
