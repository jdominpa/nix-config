{
  inputs,
  ...
}:
let
  emacsConfig =
    { pkgs, ... }:
    let
      emacs = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs-git else pkgs.emacs-git-pgtk;
    in
    {
      nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
      environment = {
        systemPackages = with pkgs; [
          ((emacsPackagesFor emacs).emacsWithPackages (
            epkgs: with epkgs; [
              ace-window
              auctex
              auto-dark
              avy
              cape
              cdlatex
              consult
              consult-denote
              consult-dir
              consult-eglot
              corfu
              denote
              diff-hl
              eglot-tempel
              eldoc-box
              embark
              embark-consult
              envrc
              exec-path-from-shell
              fontaine
              hl-todo
              jinx
              just-mode
              kind-icon
              macrostep
              magit
              magit-todos
              marginalia
              markdown-mode
              meow
              modus-themes
              nael
              nix-mode
              orderless
              org-appear
              org-modern
              org-pdftools
              pdf-tools
              popper
              rainbow-delimiters
              rustic
              tempel
              transient
              treesit-grammars.with-all-grammars
              vertico
            ]
          ))
          # Emacs LSP performance booster
          emacs-lsp-booster
          # Spellchecking backend for jinx
          hunspell
          hunspellDicts.en-us-large
          hunspellDicts.es-es
          # Nix language server
          nixd
        ];
        variables = {
          EDITOR = "emacsclient --alternate-editor='emacs' -t";
          VISUAL = "emacsclient --alternate-editor='emacs' -c";
        };
      };
    };
in
{
  flake.modules.nixos.emacs = {
    imports = [ emacsConfig ];
  };

  flake.modules.darwin.emacs = {
    imports = [ emacsConfig ];
  };

  flake.modules.homeManager.emacs = {
    home.file = {
      ".emacs.d/lisp".source = ./config/lisp;
      ".emacs.d/site-lisp".source = ./config/site-lisp;
      ".emacs.d/abbrev_defs".source = ./config/abbrev_defs;
      ".emacs.d/init.el".source = ./config/init.el;
      ".emacs.d/templates".source = ./config/templates;
    };
  };
}
