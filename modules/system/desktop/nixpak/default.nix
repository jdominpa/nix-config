{
  inputs,
  ...
}:
{
  flake.modules.nixos.nixpak =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (config) i18n time;
      cfg = config.security.nixpak;
      # Icon themes of the system, for the cursor theme
      icons = "/run/current-system/sw/share/icons";

      waylandSecurityContext = pkgs.stdenv.mkDerivation {
        name = "wayland-security-context";
        src = ./wayland-security-context.c;
        dontUnpack = true;
        nativeBuildInputs = [
          pkgs.pkg-config
          pkgs.wayland-scanner
        ];
        buildInputs = [ pkgs.wayland ];
        buildPhase = ''
          protocol=${pkgs.wayland-protocols}/share/wayland-protocols/staging/security-context/security-context-v1.xml
          wayland-scanner client-header $protocol security-context-v1-client-protocol.h
          wayland-scanner private-code $protocol security-context-v1-protocol.c
          $CC -O2 -Wall -Wextra -Werror -I. $src security-context-v1-protocol.c \
            $(pkg-config --cflags --libs wayland-client) -o wayland-security-context
        '';
        installPhase = ''
          install -Dm755 -t $out/bin wayland-security-context
        '';
      };

      # The sandbox takes its Wayland socket from `NIXPAK_WAYLAND_SOCKET`, so
      # its launcher has to be started through the helper that creates the
      # socket. Started on its own it refuses to run. This copies the package
      # nixpak installs and points everything that starts the sandbox (the
      # executable, desktop entries) at a launcher that goes through the
      # helper.
      restrictWayland =
        sandbox:
        let
          name = baseNameOf sandbox.app.binPath;
          script = "${sandbox.script}/bin/${name}";
          launcher = pkgs.writeShellScript name ''
            exec ${waylandSecurityContext}/bin/wayland-security-context \
              ${lib.escapeShellArg sandbox.flatpak.appId} ${script} "$@"
          '';
        in
        pkgs.runCommandLocal "${sandbox.env.name}-restricted-wayland" { } ''
          cp -rsL --no-preserve=mode ${sandbox.env} $out
          { grep -RlF --binary-files=without-match ${script} $out || true; } | while read -r file; do
            cp --remove-destination "$(readlink -f "$file")" "$file"
            substituteInPlace "$file" --replace-fail ${script} ${launcher}
          done
          ln -sf ${launcher} $out/bin/${name}
        '';
    in
    {
      imports = [ inputs.nixpak.nixosModules.default ];

      # These take precedence over the packages nixpak installs
      environment.systemPackages = lib.pipe cfg.apps [
        (lib.filterAttrs (_: app: app.expose))
        (lib.mapAttrsToList (_: app: lib.hiPrio (restrictWayland app.output.config)))
      ];

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
              etc.sslCertificates.enable = true;
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
                sockets.pulse = lib.mkDefault true;
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
                    # `sloth.env` fails when the variable is not set, so the
                    # sandbox never starts without the restricted socket
                    [
                      (sloth.env "NIXPAK_WAYLAND_SOCKET")
                      (sloth.concat' sloth.runtimeDir "/wayland-0")
                    ]
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
                  WAYLAND_DISPLAY = "wayland-0";
                }
                // i18n.extraLocaleSettings
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
