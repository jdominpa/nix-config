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

  perSystem = { lib, pkgs, self', ... }: {
    packages.emacs = inputs.wrappers.wrappers.emacs.wrap {
      inherit pkgs;
      package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs-git else pkgs.emacs-git-pgtk;
      # The wrapper module adds a variant wrapping `bin/emacs-${package.emacs.version}`.
      # That name only exists for nixpkgs' releases, where the derivation version
      # matches emacs' own version; emacs-overlay versions its builds by date, so
      # the variant looks for a `bin/emacs-20260707.0` that was never built.
      # `bin/emacs` and `bin/emacsclient` are wrapped either way.
      wrapperVariants = lib.mkForce { };
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
      # `user-emacs-directory` stays writable (backups, autosaves, recentf,
      # savehist, eln-cache); the read-only parts are read from the store
      # through `+core-config-directory`.
      userDirectory = "~/.emacs.d/";
      earlyConfigFile = ''
        (defvar +core-config-directory "${./config}/"
          "Directory holding the read-only parts of the configuration.")
      ''
      + builtins.readFile ./config/early-init.el;
      configFile = builtins.readFile ./config/init.el;
    };
  };
}
