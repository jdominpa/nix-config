{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      ace-window
      auctex
      avy
      cape
      cdlatex
      consult
      consult-dir
      consult-eglot
      corfu
      diff-hl
      fontaine
      hl-todo
      jinx
      kind-icon
      macrostep
      magit
      marginalia
      meow
      mini-echo
      modus-themes
      nix-mode
      orderless
      pdf-tools
      rainbow-delimiters
      spacious-padding
      vertico
      which-key
      yasnippet
    ];
  };
}
