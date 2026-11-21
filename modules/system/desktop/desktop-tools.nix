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
          bubblewrap = {
            network = true;
            sockets.pulse = true;
            bind = {
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
        };
      # Sandboxed localsend
      security.nixpak.apps.localsend.configuration =
        { sloth, ... }:
        {
          app.package = pkgs.localsend;
          flatpak.appId = "org.localsend.localsend_app";
          portals = [
            "FileChooser"
            "Notification"
            "OpenURI"
          ];
          # Discovery needs multicast, which pasta cannot pass through
          pasta.enable = false;
          bubblewrap = {
            network = true;
            bind = {
              rw = [
                # Settings, device name and TLS identity (`shared_preferences.json`)
                (sloth.mkdir (sloth.concat' sloth.xdgDataHome "/org.localsend.localsend_app"))
                (sloth.mkdir sloth.xdgDownloadDir)
              ];
            };
          };
        };
      # Transfers and multicast discovery of the LocalSend sandbox
      networking.firewall = {
        allowedTCPPorts = [ 53317 ];
        allowedUDPPorts = [ 53317 ];
      };

      programs = {
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
