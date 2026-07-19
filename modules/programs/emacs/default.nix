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
              avy
              browse-at-remote
              cape
              cdlatex
              consult
              consult-denote
              consult-dir
              consult-eglot
              corfu
              dape
              denote
              diff-hl
              eglot-tempel
              eldoc-box
              embark
              embark-consult
              envrc
              exec-path-from-shell
              fontaine
              ghostel
              goggles
              hl-todo
              indent-bars
              jinx
              just-ts-mode
              kind-icon
              magit
              magit-todos
              marginalia
              meow
              modus-themes
              mu4e
              nael
              nix-ts-mode
              orderless
              org-appear
              org-modern
              org-pdftools
              pdf-tools
              popper
              rainbow-delimiters
              rainbow-mode
              rustic
              spacious-padding
              tempel
              treesit-grammars.with-all-grammars
              vertico
              ws-butler
            ]
          ))
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
      ".emacs.d/modules".source = ./config/modules;
      ".emacs.d/abbrev_defs".source = ./config/abbrev_defs;
      ".emacs.d/early-init.el".source = ./config/early-init.el;
      ".emacs.d/init.el".source = ./config/init.el;
      ".emacs.d/templates".source = ./config/templates;
    };
  };
}
