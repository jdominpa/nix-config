{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.home.cli.kanata;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    cli.kanata.enable = mkEnableOption "Enable Kanata (keyboard remapper).";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = [ pkgs.kanata ];
    };
  };
}
