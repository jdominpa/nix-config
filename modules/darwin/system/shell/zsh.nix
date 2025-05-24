{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.system.shell.zsh;
  inherit (config.jdp.darwin.system) homebrew;
in
{
  options.jdp.darwin = {
    system.shell.zsh.enable = lib.mkEnableOption "Enable Zsh shell.";
  };

  config = lib.mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      shellInit = lib.mkIf homebrew.enable ''
        eval "$(/opt/homebrew/bin/brew shellenv)"
      '';
    };
  };
}
