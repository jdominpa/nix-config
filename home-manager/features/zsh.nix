{
  config,
  pkgs,
  ...
}: {
  home.packages = [pkgs.starship];
  programs = {
    starship.enable = true;
    zsh = {
      enable = true;
      dotDir = ".config/zsh";
      history = {
        size = 10000;
        save = 10000;
        path = "${config.xdg.dataHome}/zsh/history";
      };
      autocd = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
    };
  };
}
