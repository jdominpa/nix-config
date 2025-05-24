{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.home.editors.emacs;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    editors.emacs = {
      enable = lib.mkEnableOption "Enable personal emacs configuration.";
      withLsp = lib.mkEnableOption "Install language servers for emacs.";
    };
  };

  config = lib.mkIf cfg.enable {
    warnings =
      lib.optional (!config.jdp.base.system.editors.emacs.enable)
        "`jdp.home.editors.emacs` is enabled but emacs is not installed. Consider enabling `jdp.base.system.editors.emacs`.";

    home-manager.users.${user.name} = {
      home.file = {
        ".emacs.d/lisp".source = lib.jdp.relativeToRoot "config/emacs/lisp";
        ".emacs.d/site-lisp".source = lib.jdp.relativeToRoot "config/emacs/site-lisp";
        ".emacs.d/snippets".source = lib.jdp.relativeToRoot "config/emacs/snippets";
        ".emacs.d/abbrev_defs".source = lib.jdp.relativeToRoot "config/emacs/abbrev_defs";
        ".emacs.d/init.el".source = lib.jdp.relativeToRoot "config/emacs/init.el";
      };

      # LSP servers
      home.packages = lib.optionals cfg.withLsp (
        with pkgs;
        [
          nixd
        ]
      );
    };
  };
}
