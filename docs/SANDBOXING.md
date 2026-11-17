# Application sandboxing

Selected desktop applications run in [nixpak](https://github.com/nixpak/nixpak)
sandboxes (bubblewrap, `xdg-dbus-proxy`, `pasta` and a restricted Wayland
socket). The shared policy is in
[nixpak/default.nix](../modules/system/desktop/nixpak/default.nix); each
application declares only what it needs next to its own package configuration.

## Why these applications are sandboxed

The scope is driven by blast radius and exposure to untrusted input, not by
treating the applications as malicious:

- **Brave** renders arbitrary web content and runs third-party extensions. A
  renderer or extension compromise would otherwise inherit the home directory,
  the session bus and the network.
- **Vesktop** is an Electron chat client that displays content from strangers
  (images, links, embeds) and runs community plugins.
- **mpv** parses arbitrary media files and streams.
- **LocalSend** accepts connections and files from any device on the LAN.

Each sandbox grants only the paths, sockets, portals and network its documented
use needs.

## Shared boundaries

Every application declared in `security.nixpak.apps` gets the `defaults` of
`nixpak/default.nix`. They hold what every graphical application needs (display,
fonts, GPU); a privilege only some need (network, audio, portals, D-Bus names)
is opt-in, so each sandbox's definition lists what it gets.

- **Filesystem:** The root is a fresh tmpfs and `/nix/store` is read-only. The
  real home directory, `/etc`, `/var`, `/run` and `/root` are not there; only
  the paths an application lists are bind-mounted, plus read-only host
  fontconfig, time zone, CA bundle and the system icon themes (for the cursor),
  and the GPU files below. Binds use `--bind-try`, so a source
  that does not exist at launch is skipped silently: create directories the
  application needs with `sloth.mkdir`.
- **`/tmp`** is `$XDG_RUNTIME_DIR/nixpak-<appId>-tmp`, shared by the instances
  of one application only and gone at logout.
- **Environment** is cleared (`--clearenv`) and a short fixed list is set by the
  policy: `HOME`, `USER`, the `XDG_*` session variables, locale and time zone,
  the cursor theme, and the display and bus addresses. With the OpenURI portal,
  also `PATH` (`xdg-utils` alone) and `NIXOS_XDG_OPEN_USE_PORTAL`, so that
  `xdg-open` goes through the portal.
- **Namespaces:** User, IPC, PID, UTS and cgroup are unshared; `--new-session`
  blocks `TIOCSTI` keystroke injection into the terminal and `--die-with-parent`
  ties the sandbox to its launcher.
- **D-Bus:** Only a filtered copy of the session bus (`xdg-dbus-proxy`). There
  is no system bus (`/run/dbus` does not exist), no systemd user bus, no secret
  service, no SSH or GPG agent. An application may own `<appId>` and
  `<appId>.*`; every other name it talks to is listed explicitly.
- **Portals:** Calls to `org.freedesktop.portal.Desktop` are allowed per
  interface through the `portals` option. Reading settings and closing its own
  requests and sessions are always allowed.
- **Portal identity:** nixpak writes a `/.flatpak-info`, so
  `xdg-desktop-portal` treats the process as a Flatpak application: files
  chosen in the file chooser arrive through the document portal, and the
  Secret portal hands out a key per application ID.
- **Network:** None by default (`bubblewrap.network`, opt-in). An application
  that opts in gets `pasta` in isolate mode: a loopback of its own (host
  services on `127.0.0.1` are out of reach), a fixed fake address, no inbound
  connections and no port forwarding. Internet and LAN work. An application
  that needs the host network also turns `pasta` off.
- **GPU:** On by default (`gpu.enable`): `/dev/dri`, the NVIDIA device nodes
  (`/dev/nvidia0`, `nvidiactl`, `nvidia-modeset`, `nvidia-uvm`), the NVIDIA EGL
  configuration in `/etc/egl`, `/run/opengl-driver` and the PCI sysfs. An
  application that sets `gpu.enable = false` gets none of them.
- **Audio:** None by default. Each application opts in to the PulseAudio
  socket (`bubblewrap.sockets.pulse`, which allows recording) and the PipeWire
  socket (`bubblewrap.sockets.pipewire`); the table below shows which.
- **Display:** A restricted Wayland socket; see [Wayland](#wayland).
- **Not granted:** X11, `/dev/input`, raw USB, serial or camera devices.

## Applications

|Application|App ID                       |Network            |Wayland   |Audio               |
|-----------|-----------------------------|-------------------|----------|--------------------|
|Vesktop    |`dev.vencord.Vesktop`        |pasta              |restricted|PulseAudio, PipeWire|
|Brave      |`org.chromium.Chromium`      |pasta, 2 host ports|restricted|PulseAudio          |
|mpv        |`io.mpv.Mpv`                 |pasta              |restricted|PulseAudio          |
|LocalSend  |`org.localsend.localsend_app`|host network       |restricted|none                |

Definitions: [discord.nix](../modules/features/discord.nix),
[brave.nix](../modules/features/brave.nix) and the `mpv` and `localsend` blocks
of [desktop-tools.nix](../modules/system/desktop/desktop-tools.nix).

### Vesktop

- Writable: `~/.config/vesktop`; `~/.cache/vesktop` mounted as the sandbox's
  `~/.cache` (shader and font caches).
- Portals: FileChooser, Inhibit, Notification, OpenURI, ScreenCast.
- D-Bus: `org.freedesktop.Notifications` and `org.kde.StatusNotifierWatcher`
  (tray icon).
- The PipeWire socket is mounted for screen-share audio (WA-007, see
  [Limits](#limits)). Without `bubblewrap.sockets.pipewire`, screen sharing has
  no audio.

### Brave

- Writable: `~/.config/BraveSoftware/Brave-Browser`,
  `~/.cache/BraveSoftware/Brave-Browser` and `~/Downloads`. Other files come
  through the file chooser.
- Portals: FileChooser, Inhibit (the screen stays on during video),
  Notification, OpenURI, Print, ScreenCast, Secret. The Camera portal is not
  granted, so web pages cannot use the webcam.
- D-Bus: `org.freedesktop.Notifications`, and ownership of
  `org.mpris.MediaPlayer2.brave.*` for media controls in the bar.
- Two host loopback ports are forwarded into its `pasta` network: 8384 (the
  Syncthing GUI) and 11987 (CoolerControl). Other host services, local
  development servers included, and the host's abstract Unix sockets are out of
  reach (checked, [Audits](#audits)).
- Chromium's own sandbox for web content runs inside this one.
- The app ID is `org.chromium.Chromium`, not Brave's. See
  [Browser secrets](#browser-secrets).

### mpv

- Writable: `~/.config/mpv`, `~/.local/state/mpv` (watch-later positions) and
  `~/.cache/mpv`.
- Read-only: `~/Documents`, `~/Downloads`, `~/Pictures`. mpv opens no file
  chooser, so it cannot play files anywhere else (`~/dev`, `~/GrapheneOS`, USB
  drives). `umpv` starts a new sandbox each time instead of queueing into the
  running player.
- No portals beyond reading settings, no D-Bus names beyond its own. mpv
  needs the network to play streams.

### LocalSend

- Writable: `~/.local/share/org.localsend.localsend_app` (settings, device name
  and TLS identity) and `~/Downloads`, where it receives. Files to send come
  through the file chooser.
- Portals: FileChooser, Notification, OpenURI. No audio socket.
- **Shares the host network** (`pasta` off, WA-009 in
  [WORKAROUNDS.md](WORKAROUNDS.md)). Discovery is UDP multicast on
  `224.0.0.167:53317` and pasta has no IGMP proxy, so inside pasta the sandbox
  and other devices do not see each other. The firewall opens 53317/tcp and
  53317/udp for it. What the host network exposes is under [Limits](#limits).
- Not checked on the switched system ([Audits](#audits)).

## Wayland

A client that connects to niri's own socket is offered, among others (observed
with `wayland-info` inside a sandbox on niri 26.04):

- `zwlr_screencopy_manager_v1`: capture the screen without any prompt.
- `zwp_virtual_keyboard_manager_v1` and `zwlr_virtual_pointer_manager_v1`:
  inject keystrokes and clicks into any window, a full sandbox escape through a
  terminal.
- `zwlr_data_control_manager_v1` and `ext_data_control_manager_v1`: read and
  write the clipboard in the background.
- `zwlr_layer_shell_v1` and `ext_session_lock_manager_v1`: draw over the screen,
  or lock it.
- the toplevel list, workspace, output management, gamma control and input
  method managers.

Sandboxes never get that socket. Each launcher starts through
`wayland-security-context`, a small helper in
[wayland-security-context.c](../modules/system/desktop/nixpak/wayland-security-context.c).
It asks niri for a new socket through `wp_security_context_v1` (what Flatpak
does), tagged with the app ID, and the sandbox mounts that socket as its
`wayland-0`. Niri marks the clients of such a socket as sandboxed and hides the
privileged protocols from them, but they still talk to it directly, so
`zwp_linux_dmabuf_v1` stays and GPU rendering is not copied through shared
memory.

- Every way of starting a sandbox goes through the helper: the executables in
  `PATH` and the desktop entries are rewritten to call it, and the bare nixpak
  launcher refuses to start without `NIXPAK_WAYLAND_SOCKET`.
- The helper never falls back to niri's own socket: if the compositor does not
  offer `wp_security_context_v1` the application does not start.
- The socket lives as long as any process of the sandbox does; the helper
  removes its path in `$XDG_RUNTIME_DIR` when the launched command exits.

The restricted socket has been checked with `wayland-info`, mpv, Brave and
Vesktop ([Audits](#audits)).

## Browser secrets

Brave encrypts cookies and saved passwords with a key it asks the **Secret
portal** for. The portal stores that key per application ID, and the profile in
use was created when Brave ran unsandboxed and registered as
`org.chromium.Chromium`. The sandbox uses the same ID so that the existing
profile stays readable; any other ID would silently log everything out (WA-008
in [WORKAROUNDS.md](WORKAROUNDS.md)).

- No `org.freedesktop.secrets` access and no keyring grant: only the portal
  method `Secret.RetrieveSecret`.
- If the portal is unavailable Chromium can fall back to a weaker key. Check
  `os_crypt.portal.prev_init_success` in `Local State` of the profile.
- Bookmarks, history, extension data and the profile itself stay readable by
  Brave. The sandbox isolates them from other applications, not from a
  compromised Brave, and it does not protect them from unsandboxed
  applications.

## Limits

- **The restricted Wayland socket relies on niri's list** of privileged
  protocols. What niri considers unprivileged (focused keyboard input, the
  clipboard of the focused window, the cursor) is still offered.
- **PulseAudio and PipeWire** grant recording. Vesktop's PipeWire socket also
  lets it list and record other applications' streams and the camera.
- **pasta isolates loopback, not the LAN.** From a sandbox, a TCP connection to
  the LAN address of the host, on Syncthing's sync port, succeeded: anything the
  host listens on at `0.0.0.0` is reachable. Syncthing authenticates by device
  certificate.
- **Brave's forwarded ports** reach Syncthing's GUI (needs credentials) and
  CoolerControl (needs a password).
- **LocalSend shares the host network**, so it reaches every service on the
  host's loopback and the host's abstract Unix sockets. From the host network
  namespace, the X11 socket `@/tmp/.X11-unix/X0` accepted a connection without
  authentication (2026-11-16, Brave audit); from LocalSend that would allow
  reading and injecting input in X11 windows such as Steam's. Not checked from
  the LocalSend sandbox itself.
- **No seccomp filter** beyond what an application sets for itself, and the
  whole `/nix/store` is readable.
- **Portal grants are real grants.** A compromised application can still do
  whatever its portals allow: open links, read a user-chosen file, share the
  screen after the user picks a source.
- **Downloads.** `~/Downloads` is writable by Brave and LocalSend and readable
  by mpv, so a compromised Brave or LocalSend can plant files there.

## Adding an application

Declare `security.nixpak.apps.<name>.configuration` where the package is
configured (see the existing ones). Set `app.package` and `flatpak.appId`, then
grant only what the use needs:

|Option                                             |Purpose                                          |
|---------------------------------------------------|-------------------------------------------------|
|`portals = [ "FileChooser" … ]`                    |Portal interfaces the application may call       |
|`dbus.policies."<name>" = "talk"`                  |Bus names it may talk to (`see`, `talk`, `own`)  |
|`bubblewrap.bind.rw` / `.ro`                       |Paths to mount; use `sloth.mkdir` for directories|
|`bubblewrap.network = true`                        |Network, through `pasta` unless that is turned off|
|`pasta.args = lib.mkAfter [ "--tcp-ns" "<ports>" ]`|Host loopback ports reachable from the sandbox   |
|`gpu.enable = false`                               |No GPU devices or drivers                        |
|`bubblewrap.sockets.pulse`                         |PulseAudio socket (playback and recording)       |
|`bubblewrap.sockets.pipewire`                      |PipeWire socket                                  |

Then add the application to the table above and to its section, and to
[SECURITY.md](SECURITY.md) if it changes a boundary.

## Audits

Each check holds for the configuration it ran against: rerun it after a change
to the shared policy, the helper, niri or the app's sandbox.

To check:

- alpha, mpv without `PATH`: the policy no longer sets `PATH` for sandboxes
  without the OpenURI portal. mpv still plays a local file and a stream through
  `yt-dlp` (its wrapper puts `yt-dlp` on `PATH`).
- alpha, LocalSend discovery and transfer: with the sandbox running, alpha
  appears on the phone and the phone on alpha; a file sent each way arrives (on
  alpha, in `~/Downloads`).
- alpha, LocalSend on the restricted socket: `/proc/<pid>/mountinfo` of the
  running `localsend_app` shows the helper's `nixpak-wayland-<pid>` socket as
  `wayland-0`; `niri msg windows` lists it.
- alpha, LocalSend sandbox contents: `nsenter` into the running sandbox. Only
  the two writable paths are in the home directory, the device name survives a
  restart, and `xdpyinfo` against `@/tmp/.X11-unix/X0` shows whether the X11
  limit applies.

|Date      |Host |Check                                               |How                                                                                                   |Findings                                                                                                                                                                             |
|----------|-----|----------------------------------------------------|------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|2026-11-14|alpha|Privileged protocols hidden on the restricted socket|`wayland-info` through the helper                                                                     |`zwp_linux_dmabuf_v1` still offered; screencopy, virtual keyboard and pointer, both data-control managers, layer shell, session lock and `wp_security_context_manager_v1` not offered|
|2026-11-14|alpha|GPU rendering on the restricted socket              |mpv playing a test stream through Vulkan                                                              |Played, with `zwp_linux_dmabuf_v1` bound                                                                                                                                             |
|2026-11-16|alpha|Brave on the restricted socket                      |`/proc/<pid>/mountinfo` of the running Brave; `niri msg windows`                                      |Its `wayland-0` is the helper's `nixpak-wayland-<pid>` socket; a native Wayland window, playing video                                                                                |
|2026-11-16|alpha|Abstract Unix sockets reachable from Brave          |`ss -xl` on the host; `xdpyinfo` run inside the namespaces of the running Brave with `nsenter`        |Same network namespace as the host. `@/tmp/.X11-unix/X0` is listening and `xdpyinfo` connected without authentication, though the socket file is not mounted in the sandbox          |
|2026-11-17|alpha|Vesktop on the restricted socket                    |`/proc/<pid>/mountinfo` of the running Vesktop; `niri msg windows`                                    |Its `wayland-0` is the helper's `nixpak-wayland-<pid>` socket; a native Wayland window (`--ozone-platform=wayland`)                                                                  |
|2026-11-17|alpha|Brave's `pasta` arguments, outside the sandbox      |`pasta` run by hand with the arguments evaluated for Brave; `/dev/tcp` to host ports, `/proc/net/unix`|Host ports 8384 and 11987 reachable, 631 and 22000 not; no abstract X11 socket; internet reachable. pasta rejects the ports if they come before `--tcp-ns none`                      |
|2026-11-17|alpha|Running Brave with `--tcp-ns 8384,11987`            |`nsenter` into its namespaces; `/dev/tcp` to `127.0.0.1` and `::1`; `/proc/net/unix`                  |`127.0.0.1:8384` and `:11987` answer `200`, 631 and 22000 closed; `[::1]:8384` is accepted and reset, so `localhost:8384` fails in Brave; no abstract X11 socket                     |
