{
  myLib,
  pkgs,
  ...
}: {
  jdp.home = {
    homeDirectory = "/Users/${myLib.vars.username}";
    cli = {
      fzf.enable = true;
      git.enable = true;
      starship.enable = true;
      zsh.enable = true;
    };
    editors.emacs.enable = true;
  };
}
