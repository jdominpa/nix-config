{
  myLib,
  pkgs,
  ...
}:
{
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

  # Homebrew's default install location:
  #   /opt/homebrew for Apple Silicon
  #   /usr/local for macOS Intel
  # The prefix /opt/homebrew was chosen to allow installations
  # in /opt/homebrew for Apple Silicon and /usr/local for Rosetta 2 to coexist and use bottles.
  programs.zsh = {
    envExtra = ''
      export PATH="$PATH:/opt/homebrew/bin:/usr/local/bin"
    '';
  };
}
