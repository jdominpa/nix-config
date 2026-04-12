{
  flake.modules.homeManager.starship =
    { lib, ... }:
    {
      programs.starship = {
        enable = true;
        enableZshIntegration = true;
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
            format = "\\[$branch(:$remote_branch) ";
          };
          git_commit = {
            format = "$hash ";
          };
          git_status = {
            format = "$all_status$ahead_behind\\]";
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
    };
}
