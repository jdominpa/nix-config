{
  home.file = {
    ".emacs.d/jdp-core".source = ../../../config/emacs/jdp-core;
    ".emacs.d/snippets".source = ../../../config/emacs/snippets;
    ".emacs.d/early-init.el".source = ../../../config/emacs/early-init.el;
    ".emacs.d/init.el".source = ../../../config/emacs/init.el;
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs:
      with epkgs; [
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
        markdown-mode
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
