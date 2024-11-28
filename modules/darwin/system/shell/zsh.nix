{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.shell.zsh;
in
{
  options.jdp.darwin = {
    system.shell.zsh.enable = mkEnableOption "Enable Zsh shell.";
  };

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
    };
  };
}
