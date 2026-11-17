{
  flake.modules.nixos.brave =
    { lib, pkgs, ... }:
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
          # Only forward the Syncthing GUI and CoolerControl ports. Added after
          # nixpak's `--tcp-ns none`; pasta rejects them before it.
          pasta.args = lib.mkAfter [
            "--tcp-ns"
            "8384,11987"
          ];
          bubblewrap = {
            sockets.pulse = true;
            bind.rw = [
              (sloth.mkdir (sloth.concat' sloth.homeDir "/.config/BraveSoftware/Brave-Browser"))
              (sloth.mkdir (sloth.concat' sloth.xdgCacheHome "/BraveSoftware/Brave-Browser"))
              (sloth.mkdir sloth.xdgDownloadDir)
            ];
          };
        };
    };

  flake.modules.darwin.brave =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.brave ];
    };
}
