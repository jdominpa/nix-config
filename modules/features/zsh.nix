{
  lib,
  inputs,
  moduleWithSystem,
  ...
}:
{
  flake.nixosModules.zsh = moduleWithSystem ({
    self',
    ...
  }: {
    programs.zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
      autosuggestions.enable = true;
      # `compinit` is run from the wrapper's zshrc with an explicit, writable
      # dumpfile. The global one would dump next to the read-only ZDOTDIR.
      enableGlobalCompInit = false;
    };
    # `programs.zsh.enable` puts plain `pkgs.zsh` in the system profile, which
    # collides with the wrapper's `bin/zsh`. hiPrio makes the wrapper win.
    environment.systemPackages = [ (lib.hiPrio self'.packages.zsh) ];
    users.defaultUserShell = self'.packages.zsh;
  });

  flake.darwinModules.zsh = moduleWithSystem ({
    config,
    self',
    ...
  }: {
    programs.zsh = {
      enable = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      # See the NixOS module above.
      enableGlobalCompInit = false;
      shellInit = lib.mkIf config.homebrew.enable ''
        eval "$(/opt/homebrew/bin/brew shellenv)"
      '';
    };
    environment.systemPackages = [ (lib.hiPrio self'.packages.zsh) ];
    # Adds /run/current-system/sw/bin/zsh to /etc/shells so `chsh` accepts it.
    environment.shells = [ self'.packages.zsh ];
  });

  perSystem = { pkgs, self', ... }: {
    packages = {
      starship = inputs.wrappers.wrappers.starship.wrap {
        inherit pkgs;
        settings = {
          add_newline = false;
          format = lib.concatStrings [
            "$directory"
            "$jobs"
            "$shlvl"
            "$character"
          ];
          right_format = lib.concatStrings [
            "$cmd_duration"
            "$git_branch"
            "$git_commit"
            "$git_status"
          ];
          continuation_prompt = "❯";
          directory = {
            format = "[$path]($style)[$read_only]($read_only_style) ";
            read_only = "%";
            read_only_style = "bold red";
          };
          jobs = {
            format = "[$symbol]($style)";
            style = "bold yellow";
            symbol = "*";
            symbol_threshold = 1;
          };
          shlvl = {
            disabled = false;
            format = "[$symbol]($style)";
            style = "bold green";
            repeat = true;
            symbol = "❯"; # HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT (U+276F)
            repeat_offset = 2;
          };
          cmd_duration = {
            format = "[$duration]($style) ";
            style = "italic cyan";
          };
          git_branch = {
            format = "\\[$branch(:$remote_branch)";
          };
          git_commit = {
            format = " $hash";
          };
          git_status = {
            format = "( $all_status$ahead_behind)\\]";
            ahead = "[↑](bold white)";    # UPWARDS ARROW (U+2191)
            behind = "[↓](bold white)";   # DOWNWARDS ARROW (U+2193)
            diverged = "[⇕](bold white)"; # UP DOWN DOUBLE ARROW (U+21D5)
            untracked = "[?](bold blue)";
            stashed = "[\\$](bold yellow)";
            modified = "[!](bold red)";
            staged = "[+](bold green)";
            renamed = "";
            deleted = "";
          };
        };
      };
      zsh = inputs.wrappers.wrappers.zsh.wrap {
        inherit pkgs;
        runtimePkgs = [ pkgs.devenv pkgs.fzf ];
        zshAliases = {
          cat = lib.getExe pkgs.bat;
          grep = lib.getExe pkgs.ripgrep;
          ls = "${lib.getExe pkgs.lsd}";
          ll = "ls -l";
          la = "ls -la";
        };
        # Bitwarden ssh auth socket environment variable
        zshenv.content = ''
          [[ -S $HOME/.bitwarden-ssh-agent.sock ]] && export SSH_AUTH_SOCK=$HOME/.bitwarden-ssh-agent.sock
        '';
        zshrc.content = ''
          #
          # Writable state
          #

          # The wrapper points ZDOTDIR at a read-only store path, so everything
          # zsh would write relative to it (history, the completion dump, cdr's
          # recent directories) has to be given a writable location explicitly.
          zsh_cache_dir=''${XDG_CACHE_HOME:-$HOME/.cache}/zsh
          zsh_data_dir=''${XDG_DATA_HOME:-$HOME/.local/share}/zsh
          [[ -d $zsh_cache_dir && -d $zsh_data_dir ]] ||
            mkdir -p $zsh_cache_dir $zsh_data_dir

          #
          # History
          #

          HISTFILE=$zsh_data_dir/history
          HISTSIZE=10000
          SAVEHIST=10000

          #
          # Completion
          #

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

          # The default dumpfile is ''${ZDOTDIR}/.zcompdump, which is read-only here.
          autoload -Uz compinit
          compinit -d $zsh_cache_dir/zcompdump-$ZSH_VERSION

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
          # Prompt (starship)
          #
          # `starship init` bakes the path it resolves for itself into the emitted
          # script (`which starship`, falling back to its own real argv[0]), which
          # is never the wrapper -- so the wrapper's STARSHIP_CONFIG would not
          # reach the process that actually renders the prompt. Export it here.
          export STARSHIP_CONFIG="${self'.packages.starship.configuration.constructFiles."starship.toml"}"
          eval "$(${lib.getExe self'.packages.starship} init zsh)"

          #
          # Fzf
          #
          source <(${lib.getExe pkgs.fzf} --zsh)

          unset zsh_cache_dir zsh_data_dir
        '';
      };
    };
  };
}
