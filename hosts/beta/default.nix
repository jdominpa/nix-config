{
  config,
  lib,
  pkgs,
  ...
}:
let
  hostName = "beta";
  inherit (config.jdp.base) user;
in
{
  imports = [
    (lib.jdp.relativeToRoot "modules/base")
    (lib.jdp.relativeToRoot "modules/darwin")
    (lib.jdp.relativeToRoot "modules/home")
  ];

  jdp = {
    base = {
      system = {
        editors.emacs.enable = true;
        nix.enable = true;
        packages.enable = true;
        shell.aliases.enable = true;
      };
      user = {
        enable = true;
        name = "jdominpa";
        fullName = "Joan Domingo Pasarin";
        email = "jdomingopasarin@gmail.com";
        shell = pkgs.zsh;
        homeDirectory = "/Users/${user.name}";
        home-manager.enable = true;
      };
    };
    darwin = {
      apps = {
        activityMonitor.enable = true;
        finder.enable = true;
        google-chrome.enable = true;
        google-drive.enable = true;
        whatsapp.enable = true;
      };
      system = {
        dock.enable = true;
        fonts.enable = true;
        homebrew.enable = true;
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
        kanata.enable = true;
        starship.enable = true;
        zsh.enable = true;
      };
      dev = {
        tex.enable = true;
      };
      editors.emacs = {
        enable = true;
        withLsp = true;
      };
      services = {
        launchd = {
          backupToiCloud.enable = true;
        };
        syncthing.enable = true;
      };
    };
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = 6;
}
