{
  environment.shellAliases = {
    ls = "ls -h --group-directories-first";
    ll = "ls -l";
    la = "ls -la";
    ".." = "cd ..";
  };

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
    enableLsColors = true;
  };
}
