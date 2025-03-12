{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.base.system.editors.emacs;
  emacsPkg = pkgs.emacs;
in
# TODO: update this when emacs-macport is in version 30.1
# if pkgs.stdenv.isDarwin then
#   pkgs.emacs-macport
# else
#   pkgs.emacs;
{
  options.jdp.base = {
    system.editors.emacs.enable = lib.mkEnableOption "Whether to install emacs in this system.";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      ((emacsPackagesFor emacsPkg).emacsWithPackages (
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
          pdf-tools
          rainbow-delimiters
          spacious-padding
          treesit-grammars.with-all-grammars
          vertico
          which-key
          yasnippet
        ]
      ))
    ];
  };
}
