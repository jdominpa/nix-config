{
  self,
  ...
}:
let
  sharedSettings = {
    environment.shellAliases = {
      cat = "bat";
      grep = "rg";
      ls = "ls -h --color=auto --group-directories-first";
      ll = "ls -l";
      la = "ls -la";
    };
    home-manager.sharedModules = [ self.modules.homeManager.shell ];
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

  flake.modules.homeManager.shell =
    { config, ... }:
    {
      imports = [ self.modules.homeManager.starship ];
      programs.zsh = {
        enable = true;
        dotDir = config.xdg.configHome;
        history = {
          size = 10000;
          save = 10000;
          path = "${config.xdg.dataHome}/zsh/history";
        };
        autocd = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;
        initContent = ''
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

          #
          # Options
          #

          setopt AUTO_PUSHD              # [default] cd automatically pushes old dir onto dir stack
          setopt AUTO_RESUME             # allow simple commands to resume backgrounded jobs
          setopt CORRECT                 # [default] command auto-correction
          setopt CORRECT_ALL             # [default] argument auto-correction
          setopt NO_FLOW_CONTROL         # disable start (C-s) and stop (C-q) characters
          setopt NO_HIST_IGNORE_ALL_DUPS # don't filter non-contiguous duplicates from history
          setopt HIST_FIND_NO_DUPS       # don't show dupes when searching
          setopt HIST_IGNORE_DUPS        # do filter contiguous duplicates from history
          setopt HIST_IGNORE_SPACE       # [default] don't record commands starting with a space
          setopt HIST_VERIFY             # confirm history expansion (!$, !!, !foo)
          setopt IGNORE_EOF              # [default] prevent accidental C-d from exiting shell
          setopt INTERACTIVE_COMMENTS    # [default] allow comments, even in interactive shells
          setopt LIST_PACKED             # make completion lists more densely packed
          setopt MENU_COMPLETE           # auto-insert first possible ambiguous completion
          setopt NO_NOMATCH              # [default] unmatched patterns are left unchanged
          setopt PRINT_EXIT_VALUE        # [default] for non-zero exit status
          setopt PUSHD_IGNORE_DUPS       # don't push multiple copies of same dir onto stack
          setopt PUSHD_SILENT            # [default] don't print dir stack after pushing/popping
          setopt SHARE_HISTORY           # share history across shells

          #
          # Bindings
          #

          autoload -U edit-command-line
          zle -N edit-command-line
          bindkey '^x^x' edit-command-line

          #
          # Hooks
          #

          # adds `cdr` command for navigating to recent directories
          autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
          add-zsh-hook chpwd chpwd_recent_dirs

          # enable menu-style completion for cdr
          zstyle ':completion:*:*:cdr:*:*' menu selection

          # fall through to cd if cdr is passed a non-recent dir as an argument
          zstyle ':chpwd:*' recent-dirs-default true
        '';
      };
    };
}
