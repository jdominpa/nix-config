{
  inputs,
  moduleWithSystem,
  ...
}:
let
  sharedSettings = moduleWithSystem (
    { pkgs, self', ... }: {
      environment = {
        systemPackages = [
          self'.packages.emacs
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
    }
  );
in
{
  flake.nixosModules.emacs = sharedSettings;

  flake.darwinModules.emacs = sharedSettings;

  perSystem = { pkgs, self', ... }: {
    packages.emacs = inputs.wrappers.wrappers.emacs.wrap {
      inherit pkgs;
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
        self'.packages.git
        pkgs.nixd # Nix language server
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
    };
  };
}
