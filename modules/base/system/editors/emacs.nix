{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.base.system.editors.emacs;
  emacs = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs-git else pkgs.emacs-git-pgtk;
in
{
  options.jdp.base = {
    system.editors.emacs.enable = lib.mkEnableOption "Whether to install emacs in this system.";
  };

  config = lib.mkIf cfg.enable {
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
            nix-mode
            orderless
            org-appear
            org-modern
            org-pdftools
            pdf-tools
            popper
            rainbow-delimiters
            rustic
            spacious-padding
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
      ];
      variables = {
        EDITOR = "emacsclient --alternate-editor='emacs' -t";
        VISUAL = "emacsclient --alternate-editor='emacs' -c";
      };
    };
  };
}
