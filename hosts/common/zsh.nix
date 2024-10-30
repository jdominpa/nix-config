{
  environment.shellAliases = {
    ls = "ls -h --color=auto --group-directories-first";
    ll = "ls -l";
    la = "ls -la";
    ".." = "cd ..";
  };

  programs.zsh = {
    enable = true;
    histSize = 10000;
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
    setOptions = [
      "LIST_PACKED"             # Make completion lists more densely packed
      "SHARE_HISTORY"           # Share history across shells
      "HIST_IGNORE_DUPS"        # Don't add duplicate entries to history
      "MENU_COMPLETE"           # Auto-insert first possible ambiguous completion
    ];
  };

}
