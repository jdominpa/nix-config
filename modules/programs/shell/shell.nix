let
  sharedSettings = {
    environment.shellAliases = {
      ls = "ls -h --color=auto --group-directories-first";
      ll = "ls -l";
      la = "ls -la";
    };
  };
in
{
  flake.modules.nixos.shell = {
    imports = [ sharedSettings ];
    programs.zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      autosuggestions.enable = true;
    };
  };

  flake.modules.darwin.shell =
    { config, lib, ... }:
    {
      imports = [ sharedSettings ];
      programs.zsh = {
        enable = true;
        enableCompletion = true;
        enableSyntaxHighlighting = true;
        shellInit = lib.mkIf config.homebrew.enable ''
          eval "$(/opt/homebrew/bin/brew shellenv)"
        '';
      };
    };
}
