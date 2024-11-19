{config, ...}: {
  programs.zsh = {
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
    initExtra = ''
      # Make completion:
      # - Try exact (case-sensitive) match first.
      # - Then fall back to case-insensitive.
      # - Accept abbreviations after . or _ or - (ie. f.b -> foo.bar).
      # - Substring complete (ie. bar -> foobar).
      zstyle ':completion:*' matcher-list ''' '+m:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}' '+m:{_-}={-_}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

      # Colorize completions using default `ls` colors.
      zstyle ':completion:*' list-colors '''

      # Allow completion of ..<Tab> to ../ and beyond.
      zstyle -e ':completion:*' special-dirs '[[ $PREFIX = (../)#(..) ]] && reply=(..)'

      # Categorize completion suggestions with headings:
      zstyle ':completion:*' group-name '''
      zstyle ':completion:*:descriptions' format %F{default}%B%{$'\e[3m'%}--- %d ---%{$'\e[23m'%}%b%f

      # Enable keyboard navigation of completions in menu
      # (not just tab/shift-tab but cursor keys as well):
      zstyle ':completion:*' menu select
    '';
  };
}
