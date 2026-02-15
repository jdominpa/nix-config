{
  flake.modules.nixos.desktop-tools =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        mpv
        nomacs
        qalculate-gtk
      ];
      programs = {
        localsend.enable = true;
        thunar = {
          enable = true;
          plugins = with pkgs; [
            thunar-archive-plugin
            thunar-volman
          ];
        };
      };
    };

  flake.modules.darwin.desktop-tools = {
    homebrew.masApps = {
      LocalSend = 1661733229;
    };
  };
}
