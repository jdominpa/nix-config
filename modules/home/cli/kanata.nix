{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.home.cli.kanata;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    cli.kanata.enable = lib.mkEnableOption "Enable Kanata (keyboard remapper).";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.packages = [ pkgs.kanata ];
    };
  };
}
