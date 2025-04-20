{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.base.system.editors.emacs;
  emacs =
    # FIXME: native compilation currently doesn't work with macOS Sequoia 15.4
    # https://github.com/NixOS/nixpkgs/issues/395169
    if pkgs.stdenv.hostPlatform.isDarwin then
      pkgs.emacs-git.override { withNativeCompilation = false; }
    else
      pkgs.emacs-git;
in
{
  options.jdp.base = {
    system.editors.emacs.enable = lib.mkEnableOption "Whether to install emacs in this system.";
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        (emacs.pkgs.withPackages (
          epkgs: with epkgs; [
            ace-window
            auctex
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
            minions
            modus-themes
            nix-mode
            orderless
            org-appear
            org-modern
            org-pdftools
            pdf-tools
            rainbow-delimiters
            spacious-padding
            treesit-grammars.with-all-grammars
            vertico
            yasnippet
          ]
        ))
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
