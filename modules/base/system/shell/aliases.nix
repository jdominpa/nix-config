{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.base.system.shell.aliases;
in
{
  options.jdp.base = {
    system.shell.aliases.enable = lib.mkEnableOption "Enable general shell aliases.";
  };

  config = lib.mkIf cfg.enable {
    environment.shellAliases = {
      ls = "ls -h --color=auto --group-directories-first";
      ll = "ls -l";
      la = "ls -la";
    };
  };
}
