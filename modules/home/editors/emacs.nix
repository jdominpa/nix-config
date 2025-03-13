{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.home.editors.emacs;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    editors.emacs = {
      enable = mkEnableOption "Enable personal emacs configuration.";
      withLsp = mkEnableOption "Install language servers for emacs.";
    };
  };

  config = mkIf cfg.enable {
    warnings =
      if !config.jdp.base.system.editors.emacs.enable then
        [
          "`jdp.home.editors.emacs.enable` is enabled but emacs is not installed. Consider enabling `jdp.base.system.editors.emacs.enable`."
        ]
      else
        [ ];

    home-manager.users.${user.name} = {
      home.file = {
        ".emacs.d/jdp-core".source = jdp.relativeToRoot "config/emacs/jdp-core";
        ".emacs.d/jdp-lisp".source = jdp.relativeToRoot "config/emacs/jdp-lisp";
        ".emacs.d/snippets".source = jdp.relativeToRoot "config/emacs/snippets";
        ".emacs.d/early-init.el".source = jdp.relativeToRoot "config/emacs/early-init.el";
        ".emacs.d/init.el".source = jdp.relativeToRoot "config/emacs/init.el";
      };

      # LSP servers
      home.packages = optionals cfg.withLsp (
        with pkgs;
        [
          nixd
        ]
      );
    };
  };
}
