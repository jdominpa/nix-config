{
  # Brave keeps its profile, its cache and the downloads directory. The rest of
  # the home directory it sees is empty and gone when it exits; other files
  # reach it through the file chooser. Chromium's own sandbox for the web
  # content runs inside this one, as usual.
  flake.modules.nixos.brave =
    { pkgs, ... }:
    {
      security.nixpak.apps.brave.configuration =
        { sloth, ... }:
        {
          app.package = pkgs.brave;
          # Not Brave's own ID: the Secret portal hands each application a key
          # of its own, chosen by this ID, and Brave encrypts its cookies and
          # passwords with it. Unsandboxed, Brave registers as Chromium, so its
          # profile is encrypted with the key of `org.chromium.Chromium`; any
          # other ID would leave it unreadable.
          flatpak.appId = "org.chromium.Chromium";
          portals = [
            "FileChooser"
            # Keeps the screen on while a video plays
            "Inhibit"
            "Notification"
            "OpenURI"
            "Print"
            "ScreenCast"
            "Secret"
          ];
          dbus.policies = {
            "org.freedesktop.Notifications" = "talk";
            # Media controls, as `org.mpris.MediaPlayer2.brave.instance<pid>`
            "org.mpris.MediaPlayer2.brave.*" = "own";
          };
          # The localhost of the host stays reachable: the Syncthing GUI and
          # local development servers live there
          pasta.enable = false;
          bubblewrap.bind.rw = [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/.config/BraveSoftware/Brave-Browser"))
            (sloth.mkdir (sloth.concat' sloth.xdgCacheHome "/BraveSoftware/Brave-Browser"))
            (sloth.mkdir sloth.xdgDownloadDir)
          ];
        };
    };

  flake.modules.darwin.brave =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.brave ];
    };
}
