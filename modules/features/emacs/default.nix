{
  inputs,
  self,
  ...
}:
let
  install =
    { pkgs, ... }:
    {
      wrappers.emacs.enable = true;
      environment = {
        systemPackages = [
          # Spellchecking backend for jinx. Kept as system packages because
          # enchant discovers dictionaries through the profile, not through PATH.
          pkgs.hunspell
          pkgs.hunspellDicts.en-us-large
          pkgs.hunspellDicts.es-es
        ];
        variables = {
          EDITOR = "emacsclient --alternate-editor='emacs' -t";
          VISUAL = "emacsclient --alternate-editor='emacs' -c";
        };
      };
    };
in
{
  flake.modules.nixos.emacs = install;

  flake.modules.darwin.emacs = install;

  flake.wrappers.emacs =
    { pkgs, wlib, ... }:
    {
      imports = [ wlib.wrapperModules.emacs ];
      package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs31 else pkgs.emacs31-pgtk;
      emacsPackages =
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
          (trivialBuild {
            pname = "math-delimiters";
            version = "0.1";
            src = inputs.math-delimiters;
          })
          meow
          modus-themes
          mu4e
          nael
          nix-ts-mode
          orderless
          org-appear
          org-modern
          pdf-tools
          popper
          rainbow-delimiters
          rainbow-mode
          rustic
          rust-mode
          spacious-padding
          tempel
          treesit-grammars.with-all-grammars
          vertico
          ws-butler
        ];
      # Subprocesses emacs spawns. On PATH for the wrapper only, so
      # `nix run .#emacs` on any machine gets a working magit and eglot.
      runtimePkgs = [
        # Same derivation the `git` package output is built from.
        (self.wrappers.git.wrap { inherit pkgs; })
        pkgs.nixd # Nix language server
        pkgs.ripgrep
      ];
      userDirectory = "~/.emacs.d";
      earlyConfigFile = ''
        (defvar +core-config-directory "${./config}/"
          "Directory holding the read-only parts of the configuration.")

        ;; Emacs re-derives the delayed defcustoms (`package-user-dir' and friends)
        ;; from `--init-directory' before this file runs, so anything under it that
        ;; needs to be written has to be pointed at `user-emacs-directory' by hand.
        (setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
              package-gnupghome-dir (expand-file-name "elpa/gnupg" user-emacs-directory))
      ''
      + builtins.readFile ./config/early-init.el;
      configFile = builtins.readFile ./config/init.el;
      # Fix .app bundle on darwin
      wrapperVariants = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
        Emacs = {
          binDir = "Applications/Emacs.app/Contents/MacOS";
          exePath = "Applications/Emacs.app/Contents/MacOS/Emacs";
        };
      };
    };
}
