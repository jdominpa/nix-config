{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.shell.zsh;
  homebrew = config.jdp.darwin.system.homebrew;
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
      shellInit = mkIf homebrew.enable ''
        eval "$(/opt/homebrew/bin/brew shellenv)"
      '';
    };
  };
}
