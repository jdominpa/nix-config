{
  environment.shellAliases = {
    ls = "ls -h --color=auto --group-directories-first";
    ll = "ls -l";
    la = "ls -la";
  };

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
  };
}
