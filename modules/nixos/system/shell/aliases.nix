{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.system.shell.aliases;
in
{
  options.jdp.nixos = {
    system.shell.aliases.enable = mkEnableOption "Enable general shell aliases.";
  };

  config = mkIf cfg.enable {
    environment.shellAliases = {
      ls = "ls -h --color=auto --group-directories-first";
      ll = "ls -l";
      la = "ls -la";
    };
  };
}