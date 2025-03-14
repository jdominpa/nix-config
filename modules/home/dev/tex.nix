{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.home.dev.tex;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    dev.tex.enable = mkEnableOption "Install texlive (LaTeX).";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = [ pkgs.texliveFull ];
    };
  };
}
