{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.base.system.editors.emacs;
in
{
  options.jdp.base = {
    system.editors.emacs.enable = lib.mkEnableOption "Whether to install emacs in this system.";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (emacs.pkgs.withPackages (
        epkgs: with epkgs; [
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
          marginalia
          markdown-mode
          meow
          mini-echo
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
  };
}
