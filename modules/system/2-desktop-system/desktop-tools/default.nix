{
  flake.modules.nixos.desktop-tools =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        mpv
        nomacs
        qalculate-gtk
      ];
      programs.thunar = {
        enable = true;
        plugins = with pkgs; [
          thunar-archive-plugin
          thunar-volman
        ];
      };
    };

  flake.modules.darwin.desktop-tools = {
    homebrew.casks = [
      "proton-drive"
    ];
  };
}
