{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.system.shell.zsh;
in
{
  options.jdp.nixos = {
    system.shell.zsh.enable = mkEnableOption "Enable Zsh shell.";
  };

  config = mkIf cfg.enable {
    # Basic system wide settings for zsh
    programs.zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      autosuggestions.enable = true;
    };
  };
}
