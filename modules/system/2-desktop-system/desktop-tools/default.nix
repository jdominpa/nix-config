{
  flake.modules.nixos.desktop-tools =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        brightnessctl
        libsecret
        mpv
        nomacs
        playerctl
        qalculate-gtk
        seahorse
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
        xfconf.enable = true;
      };
      services = {
        gvfs.enable = true;
        tumbler.enable = true;
        udisks2.enable = true;
      };
    };

  flake.modules.darwin.desktop-tools = {
    homebrew.masApps = {
      LocalSend = 1661733229;
    };
  };
}
