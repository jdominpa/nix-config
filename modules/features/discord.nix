{
  flake.modules.nixos.discord =
    { pkgs, ... }:
    {
      # Vesktop only keeps its own configuration directory. The rest of the home
      # directory it sees is empty and gone when it exits; files reach it
      # through the file chooser.
      security.nixpak.apps.vesktop.configuration =
        { sloth, ... }:
        {
          app.package = pkgs.vesktop;
          flatpak.appId = "dev.vencord.Vesktop";
          portals = [
            "FileChooser"
            "Inhibit"
            "Notification"
            "OpenURI"
            "ScreenCast"
          ];
          dbus.policies = {
            "org.freedesktop.Notifications" = "talk";
            # Tray icon
            "org.kde.StatusNotifierWatcher" = "talk";
          };
          bubblewrap = {
            sockets.pipewire = true; # needed for screen-share audio
            bind.rw = [
              (sloth.mkdir (sloth.concat' sloth.homeDir "/.config/vesktop"))
              # Shader and font caches, rebuilt on every start otherwise
              [
                (sloth.mkdir (sloth.concat' sloth.xdgCacheHome "/vesktop"))
                (sloth.concat' sloth.homeDir "/.cache")
              ]
            ];
          };
        };
    };

  flake.modules.darwin.discord =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.vesktop ];
    };
}
