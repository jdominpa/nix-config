{
  environment.shellAliases = {
    ls = "ls -h --color=auto --group-directories-first";
    ll = "ls -l";
    la = "ls -la";
    ".." = "cd ..";
  };

  # Basic system wide settings for zsh
  programs.zsh = {
    enable = true;
    histSize = 10000;
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
  };
}
