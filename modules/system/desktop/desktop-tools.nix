{
  flake.modules.nixos.desktop-tools =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        libsecret
        loupe
        seahorse
      ];
      # Sandboxed mpv
      security.nixpak.apps.mpv.configuration =
        { sloth, ... }:
        {
          app.package = pkgs.mpv;
          flatpak.appId = "io.mpv.Mpv";
          # Renders on the GPU, which needs `linux-dmabuf`
          waylandProxy.enable = false;
          bubblewrap.bind = {
            rw = [
              (sloth.mkdir (sloth.concat' sloth.xdgConfigHome "/mpv"))
              # Positions saved with `quit-watch-later`
              (sloth.mkdir (sloth.concat' sloth.xdgStateHome "/mpv"))
              (sloth.mkdir (sloth.concat' sloth.xdgCacheHome "/mpv"))
            ];
            ro = [
              sloth.xdgDocumentsDir
              sloth.xdgDownloadDir
              sloth.xdgPicturesDir
            ];
          };
        };

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

  flake.modules.darwin.desktop-tools = { config, ... }: {
    assertions = [
      {
        assertion = config.homebrew.enable;
        message = "desktop-tools module requires homebrew on darwin";
      }
    ];

    homebrew.masApps = {
      LocalSend = 1661733229;
    };
  };
}
