{
  environment.shellAliases = {
    ls = "ls -h --color=auto --group-directories-first";
    ll = "ls -l";
    la = "ls -la";
  };

  # Basic system wide settings for zsh
  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
  };
}
