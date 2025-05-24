{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.home.dev.tex;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    dev.tex.enable = lib.mkEnableOption "Install texlive (LaTeX).";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = [ pkgs.texliveFull ];
    };
  };
}
