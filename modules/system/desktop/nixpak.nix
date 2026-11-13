{
  inputs,
  ...
}:
{
  # Application sandboxing with nixpak, a declarative wrapper around bubblewrap.
  # Applications are declared in `security.nixpak.apps` and each one gets what
  # is listed in `defaults` plus whatever it asks for itself. Apart from the Nix
  # store it sees nothing else: no home directory, no session bus other than the
  # names it was granted, no devices other than the GPU and no host loopback.
  flake.modules.nixos.nixpak =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (config) i18n time;
      # Icon themes of the system, for the cursor theme
      icons = "/run/current-system/sw/share/icons";
    in
    {
      imports = [ inputs.nixpak.nixosModules.default ];

      security.nixpak = {
        enable = true;
        defaults =
          { config, sloth, ... }:
          let
            inherit (config.flatpak) appId;
            portal = "org.freedesktop.portal";
            portalPath = "/org/freedesktop/portal/desktop";
          in
          {
            options.portals = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              example = [
                "FileChooser"
                "OpenURI"
              ];
              description = ''
                Interfaces of the desktop portal (`org.freedesktop.portal.*`)
                the application may call.
              '';
            };

            config = {
              # /dev/dri and the drivers in /run/opengl-driver
              gpu.enable = true;
              # The application talks to a Wayland proxy instead of the
              # compositor, which would hand it every privileged protocol:
              # screen capture, virtual keyboard and pointer, clipboard
              # managers, layer shell. The proxy only passes on what ordinary
              # windows need. It does not pass on `linux-dmabuf` either, so
              # frames reach the compositor through shared memory, a copy that
              # costs some CPU. Its windows have their name in the title.
              #
              # Applications that render on the GPU (browsers, video players)
              # turn it off and get the socket of the compositor instead, with
              # every protocol it offers.
              waylandProxy.enable = lib.mkDefault true;
              etc.sslCertificates.enable = true;
              # Network access goes through pasta: internet and LAN work, but
              # the sandbox has a loopback interface of its own, so whatever
              # listens only on the localhost of the host (the Syncthing GUI,
              # for one) is out of reach. What listens on every address of the
              # host can still be reached through its LAN address. Nothing can
              # connect into the sandbox either, so an application that needs
              # that turns it off.
              pasta = {
                enable = lib.mkDefault true;
                # It prints its network setup on every start otherwise
                args = [ "--quiet" ];
              };

              dbus = {
                policies = {
                  "${appId}" = "own";
                  "${appId}.*" = "own";
                };
                # Calls to the desktop portal are allowed per interface. The
                # ones listed here only read settings or close the application's
                # own requests.
                rules = {
                  call."${portal}.Desktop" = [
                    "org.freedesktop.DBus.Properties.Get@${portalPath}"
                    "org.freedesktop.DBus.Properties.GetAll@${portalPath}"
                    "${portal}.Request.Close@${portalPath}/request/*"
                    "${portal}.Session.Close@${portalPath}/session/*"
                  ]
                  ++ map (interface: "${portal}.${interface}.*@${portalPath}") ([ "Settings" ] ++ config.portals);
                  broadcast."${portal}.Desktop" = [
                    "${portal}.Settings.SettingChanged@${portalPath}"
                    "@${portalPath}/*"
                  ];
                };
              };

              bubblewrap = {
                network = true;
                # Nothing of the session environment leaks in, only `env`
                clearEnv = true;
                dieWithParent = true;
                newSession = true;
                sockets = {
                  pulse = lib.mkDefault true;
                  wayland = !config.waylandProxy.enable;
                };
                bind = {
                  rw = [
                    # A /tmp shared by the instances of one application only
                    [
                      (sloth.mkdir (sloth.concat' sloth.runtimeDir "/nixpak-${appId}-tmp"))
                      "/tmp"
                    ]
                  ]
                  # Files picked in the file chooser are exported to the
                  # application through the document portal
                  ++ lib.optional (lib.elem "FileChooser" config.portals) [
                    (sloth.concat' sloth.runtimeDir "/doc/by-app/${appId}")
                    (sloth.concat' sloth.runtimeDir "/doc")
                  ];
                  ro = [
                    # Fontconfig of the host, the fonts are in the store
                    [
                      "/etc/static/fonts"
                      "/etc/fonts"
                    ]
                    "/etc/localtime"
                    "/etc/zoneinfo"
                    # EGL configuration of the NVIDIA driver
                    "/etc/egl"
                    "/etc/static/egl"
                    icons
                  ];
                  # The NVIDIA driver needs its own device nodes besides
                  # /dev/dri
                  dev = [
                    "/dev/nvidia0"
                    "/dev/nvidiactl"
                    "/dev/nvidia-modeset"
                    "/dev/nvidia-uvm"
                  ];
                };
                env = {
                  HOME = sloth.homeDir;
                  USER = sloth.env "USER";
                  # `xdg-open` to open links, which it does through the portal
                  PATH = lib.makeBinPath [ pkgs.xdg-utils ];
                  NIXOS_XDG_OPEN_USE_PORTAL = "1";
                  XDG_RUNTIME_DIR = sloth.runtimeDir;
                  XDG_CURRENT_DESKTOP = sloth.envOr "XDG_CURRENT_DESKTOP" "niri";
                  XDG_SESSION_TYPE = "wayland";
                  # Cursor theme of the session, for the clients that draw the
                  # cursor themselves
                  XCURSOR_PATH = icons;
                  XCURSOR_SIZE = sloth.envOr "XCURSOR_SIZE" "";
                  XCURSOR_THEME = sloth.envOr "XCURSOR_THEME" "";
                  LANG = i18n.defaultLocale;
                  LOCALE_ARCHIVE = "${i18n.glibcLocales}/lib/locale/locale-archive";
                  TZDIR = "/etc/zoneinfo";
                }
                // i18n.extraLocaleSettings
                # The proxy sets it itself
                // lib.optionalAttrs (!config.waylandProxy.enable) {
                  WAYLAND_DISPLAY = sloth.envOr "WAYLAND_DISPLAY" "wayland-0";
                }
                // lib.optionalAttrs (lib.elem "FileChooser" config.portals) { GTK_USE_PORTAL = "1"; }
                # /etc/localtime is a plain file inside the sandbox, so the name
                # of the time zone cannot be read from its link target
                // lib.optionalAttrs (time.timeZone != null) { TZ = time.timeZone; };
              };
            };
          };
      };
    };
}
