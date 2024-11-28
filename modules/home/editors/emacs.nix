{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.editors.emacs;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    editors.emacs.enable = mkEnableOption "Enable personal emacs configuration.";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.file = {
        ".emacs.d/jdp-core".source = jdp.relativeToRoot "config/emacs/jdp-core";
        ".emacs.d/snippets".source = jdp.relativeToRoot "config/emacs/snippets";
        ".emacs.d/early-init.el".source = jdp.relativeToRoot "config/emacs/early-init.el";
        ".emacs.d/init.el".source = jdp.relativeToRoot "config/emacs/init.el";
      };

      programs.emacs = {
        enable = true;
        extraPackages =
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
            vertico
            which-key
            yasnippet
          ];
      };
    };
  };
}
