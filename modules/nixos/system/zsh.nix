{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.system.zsh;
in
{
  options.jdp.nixos = {
    system.zsh.enable = mkEnableOption "Enable Zsh shell.";
  };

  config = mkIf cfg.enable {
    environment.shellAliases = {
      ls = "ls -h --color=auto --group-directories-first";
      ll = "ls -l";
      la = "ls -la";
    };

    # Basic system wide settings for zsh
    programs.zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      autosuggestions.enable = true;
    };
  };
}
