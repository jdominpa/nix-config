# Security

This is a personal configuration, not a supported security product. This
document describes the intended design, the controls present in the source,
their limits and the work that remains. **A configured control is not evidence
that it is active on a running machine**.

## Threat model and priorities

Protect the data of the user (documents, pictures, etc.), desktop application
data and the ability to recover after a compromise or a bad update. The relevant
attackers include:

- Compromised applications or dependencies.
- Other devices on a network the machine joins.
- Someone with physical access to a powered-off or stolen machine.

Prioritize, in this order, **patching, reducing exposure, least privilege,
isolation and recovery**. Kernel mitigations and module deny lists are defence
in depth, not fixes for vulnerabilities. A sandbox limits what a compromised
application can reach; it does not make arbitrary code safe.

The hardening is meant to be proportionate: small, reversible and without a
noticeable performance cost. As implemented:

- user namespaces stay enabled (bubblewrap, nixpak and Chromium's own sandbox
  need them);
- no hardened kernel, no module signature enforcement, no `init_on_free`;
- every knob in [hardening.nix](../modules/system/core/hardening.nix) carries a
  comment saying what it costs and how to back it out.

## Architecture and trust boundaries

|Boundary      |Current design                                                                                                                                                                                                                                                                                                    |
|--------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Data at rest  |LUKS2 over the whole system partition (`aes-xts-plain64`, 512-bit key for new installs, `argon2id`, 5 s), with btrfs subvolumes `@`, `@nix`, `@home`, `@snapshots` and `@swap` inside, so swap is encrypted too. Unlocked by passphrase in the initrd. `allowDiscards` is on: TRIM reveals which blocks are in use|
|Mounts        |`/boot` is not encrypted; its contents are signed, and it is mounted `noexec,nosuid,nodev` with `0600`/`0700` permissions. `/home` is `nosuid,nodev`, `/.snapshots` `noexec,nosuid,nodev`, `/tmp` a tmpfs emptied at boot                                                                                         |
|Boot chain    |Lanzaboote signs the bootloader and the unified kernel images; Secure Boot is enabled with the keys of the user (`/var/lib/sbctl`). The boot menu editor is disabled, so `init=/bin/sh` cannot be appended at the menu. Ten generations are kept (`configurationLimit`)                                           |
|Firmware      |`fwupd` is enabled, which also delivers Secure Boot revocation list (dbx) updates. The firmware administrator password is a manual step ([README](../README.md)) and cannot be checked from the OS                                                                                                                |
|Kernel        |The [baseline](#kernel-and-process-baseline) below                                                                                                                                                                                                                                                                |
|Administration|Only `wheel` may execute `sudo`. The user is in `wheel` and `networkmanager`. `sshd` is disabled on both hosts; if it is enabled, the settings in `hardening.nix` allow key-only logins for the one user, no root login                                                                                           |
|Network       |Host firewall on; the open ports are listed below. Services that only need the machine itself listen on loopback                                                                                                                                                                                                  |
|Nix           |Only `wheel` may use the Nix daemon (`allowed-users`); `trusted-users` is the default (root). Every flake input is pinned in `flake.lock`. Extra binary cache: `nix-community.cachix.org`, with its key pinned. Unfree packages are allowed                                                                       |
|Session       |`noctalia-greeter` as login manager. noctalia locks the screen after 10 minutes of idle time and turns the screens off after 11; it also provides the polkit agent                                                                                                                                                |
|Desktop apps  |Vesktop, Brave and mpv run in nixpak sandboxes ([SANDBOXING.md](SANDBOXING.md)); everything else runs with the full access of the user                                                                                                                                                                            |
|Secrets       |sops-nix; user secrets and the age key are readable by any process of the user ([SECRETS.md](SECRETS.md)). Brave's cookie and password key comes from the Secret portal                                                                                                                                           |
|Backups       |restic to a Hetzner Storage Box, encrypted on the client ([BACKUPS.md](BACKUPS.md))                                                                                                                                                                                                                               |
|Recovery      |NixOS generations (10 in the boot menu; weekly garbage collection of generations older than 7 days, so about a week of rollback) and restic snapshots                                                                                                                                                            |
|macOS         |Application firewall on with stealth mode, signed system services allowed, guest login off, automatic macOS updates, Remote Login off. FileVault and the screen lock delay are manual steps ([README](../README.md)); none of this was checked on the machine                                                     |

Sources: [hardening](../modules/system/core/hardening.nix),
[secure boot](../modules/system/core/secure-boot.nix),
[disk layout](../modules/hosts/alpha/disko.nix),
[Nix settings](../modules/system/core/nix.nix),
[SSH](../modules/system/core/services/ssh.nix),
[restic](../modules/features/restic.nix),
[secrets](../modules/system/core/secrets.nix),
[noctalia](../modules/features/noctalia.nix).

## Network exposure

The firewall is enabled and everything not listed is dropped. The ports below
are opened by the modules named in the last column (checked from the evaluated
configuration, not from the running ruleset, which needs root to read).

|Ports                                            |Opened for                   |Source                                  |
|-------------------------------------------------|-----------------------------|----------------------------------------|
|22000/tcp, 22000/udp, 21027/udp                  |Syncthing sync, LAN discovery|`services.syncthing.openDefaultPorts`   |
|53317/tcp, 53317/udp                             |LocalSend                    |`programs.localsend`                    |
|27036–27037/tcp, 27031–27036/udp, 10400–10401/udp|Steam Remote Play            |`programs.steam.remotePlay.openFirewall`|

Listening sockets besides the ports above, as last observed
([Audits](#audits)): Syncthing's GUI on `127.0.0.1:8384` (TLS, requires
credentials), CUPS on `127.0.0.1:631` and `[::1]:631`, and the CoolerControl
daemon on `127.0.0.1:11987` and `[::1]:11987` (answers 401 without credentials;
make sure its default password was changed).
Loopback is not a boundary against the user's own unsandboxed applications or
against sandboxes that share the host network (Brave).

Bluetooth is powered on at boot (`hardware.bluetooth.powerOnBoot`) and CUPS is
enabled; neither is restricted further.

## Kernel and process baseline

Implemented in [hardening.nix](../modules/system/core/hardening.nix), which
holds the values; this section summarises them. Some values are already
kernel/systemd/NixOS defaults.

### Boot parameters

|Parameter                   |Effect                                                                                                    |
|----------------------------|----------------------------------------------------------------------------------------------------------|
|`slab_nomerge`              |Do not merge slab caches of similar size; limits cross-cache heap exploits                                |
|`init_on_alloc=1`           |Zero memory on allocation                                                                                 |
|`page_alloc.shuffle=1`      |Randomise page allocator free lists                                                                       |
|`randomize_kstack_offset=on`|Randomise the kernel stack offset on each syscall                                                         |
|`vsyscall=none`             |Remove the fixed-address legacy vsyscall page                                                             |
|`debugfs=off`               |Do not expose debugfs (`/sys/kernel/debug` is absent). Tools that need it, such as powertop, will complain|

`security.protectKernelImage` disables kexec and hibernation.
`security.forcePageTableIsolation` forces PTI on.

### Sysctls

|Setting                                                                |Value |Effect and compatibility                                                                                                                              |
|-----------------------------------------------------------------------|------|------------------------------------------------------------------------------------------------------------------------------------------------------|
|`kernel.kptr_restrict`                                                 |2     |Hide kernel pointers, even from privileged readers                                                                                                    |
|`kernel.dmesg_restrict`                                                |1     |Kernel log needs `CAP_SYSLOG`                                                                                                                         |
|`kernel.unprivileged_bpf_disabled`                                     |1     |`bpf()` is root only                                                                                                                                  |
|`net.core.bpf_jit_harden`                                              |2     |Harden the BPF JIT for everyone                                                                                                                       |
|`kernel.yama.ptrace_scope`                                             |1     |A process may only trace its own descendants. `gdb -p` and `strace -p` on an unrelated process need sudo; debugging a program you launch is unaffected|
|`kernel.perf_event_paranoid`                                           |2     |No kernel profiling for unprivileged users; user-space profiling works                                                                                |
|`vm.unprivileged_userfaultfd`                                          |0     |Unprivileged userfaultfd only for user-mode faults                                                                                                    |
|`dev.tty.ldisc_autoload`, `dev.tty.legacy_tiocsti`                     |0, 0  |No auto-loading of TTY line disciplines; no `TIOCSTI` keystroke injection                                                                             |
|`vm.mmap_rnd_bits`, `vm.mmap_rnd_compat_bits`                          |32, 16|Maximum ASLR entropy for `mmap`                                                                                                                       |
|`fs.protected_fifos`, `fs.protected_regular`                           |2, 2  |Stricter `O_CREAT` rules in world-writable sticky directories                                                                                         |
|`fs.protected_symlinks`, `fs.protected_hardlinks`                      |1, 1  |Mitigate cross-user link attacks                                                                                                                      |
|`net.ipv4.conf.*` and `net.ipv6.conf.*` redirects, source routing      |0     |Not a router: ignore ICMP redirects and source-routed packets, send no redirects                                                                      |
|`net.ipv4.tcp_syncookies`, `tcp_rfc1337`, `icmp_echo_ignore_broadcasts`|1     |SYN flood and TIME-WAIT assassination protection; ignore broadcast pings                                                                              |

### Module deny list

These are blocked with `install <module> /bin/false`, which also stops an
explicit `modprobe`, so an unprivileged `socket()` or `mount` cannot auto-load
them:

- network protocols: `af_802154`, `appletalk`, `atm`, `ax25`, `can`, `dccp`,
  `decnet`, `econet`, `ipx`, `n-hdlc`, `netrom`, `p8022`, `p8023`, `psnap`,
  `rds`, `rose`, `sctp`, `tipc`, `x25`, and `esp4`, `esp6`, `rxrpc` (WA-004 in
  [WORKAROUNDS.md](WORKAROUNDS.md)).
- filesystems `cramfs`, `freevxfs`, `jffs2`, and the video test driver `vivid`.

The list does not unload modules that are already loaded, does not affect
built-in code and does not stop a privileged module insertion.

## Boot integrity

- Kernel lockdown is **off**: `/sys/kernel/security/lockdown` reads `[none]
  integrity confidentiality`. Module signature enforcement is not configured
  either; the NVIDIA modules would have to be signed first.
- A TPM 2.0 is present, but nothing uses it: no measured boot and no TPM-bound
  disk unlock. The disk always needs the passphrase.

## Supply chain

- `flake.lock` pins every input. Updates are a deliberate `just update`.
- `nixpkgs` follows `nixos-unstable`: fixes arrive quickly, so do regressions.
- The extra substituter `nix-community.cachix.org` is trusted through the option
  `nix.settings`.
- `nixpak` is a flake input and runs as the user on every launch of a sandboxed
  application: treat bumps of it, `xdg-dbus-proxy` and `bubblewrap` as
  security-relevant when reading a `flake.lock` diff.
- `sops-nix` runs as root at every activation to decrypt the secrets: treat its
  bumps the same way. The `secrets` input is a private repository fetched over
  SSH. It is read as plain files (`flake = false`), so it adds ciphertext to the
  evaluation, not code.

## Backups and recovery

Restic encrypts on the client, so the Storage Box holds only ciphertext. The
Storage Box host key is pinned.

## Known gaps and remaining work

- **No mandatory access control.** AppArmor, SELinux, USBGuard and auditd are
  not configured. Systemd service sandboxing is whatever nixpkgs ships.
- **Lockdown, module signing and TPM use are not enabled** (see Boot integrity).
- **Most applications are unsandboxed**, including Steam, Emacs, Bitwarden and
  the terminal. A compromise of any of them reaches the whole home directory,
  the browser profiles, etc. For the sandbox limits see
  [SANDBOXING.md](SANDBOXING.md#limits).
- **Firewall ports for Steam Remote Play** are open only because Steam runs
  unsandboxed; if it is sandboxed again they need a second look.
- **Nothing verifies the live firewall ruleset or `sbctl verify`**
  automatically. Run the checks below after a rebuild that touches them.
- **A compromised user process can delete the backup repository**: restic runs
  as the user. Only the Storage Box snapshots undo that, within 10 days, and
  they are configured in the Hetzner Console, outside this repository.

## Checking the live system

```sh
# Secure Boot, lockdown, boot parameters
bootctl status | grep 'Secure Boot'
sudo sbctl status && sudo sbctl verify
cat /sys/kernel/security/lockdown
cat /proc/cmdline

# Sysctls (the first group needs sudo for two keys)
sudo sysctl kernel.kptr_restrict kernel.yama.ptrace_scope \
  net.core.bpf_jit_harden vm.mmap_rnd_bits

# Firewall and listening sockets
sudo nft list ruleset | grep -E 'dport|policy'
ss -tulpn

# Mounts
findmnt /boot /home /tmp

# Backup
systemctl status restic-backups-storagebox.timer
journalctl -u restic-backups-storagebox.service -n 50
restic-storagebox snapshots
```

## Audits

Results of the checks above; those of the secrets, sandboxes and backups are in
their own documents. Each holds for the configuration it ran against.

|Date      |Host |Check                                       |How                    |Status |Findings                                                |
|----------|-----|--------------------------------------------|-----------------------|-------|--------------------------------------------------------|
|2026-11-13|alpha|Listening sockets besides the firewall ports|Not recorded           |checked|Only the loopback services listed under Network exposure|
|—         |alpha|Live firewall ruleset                       |`sudo nft list ruleset`|pending|The port table comes from the evaluated configuration   |
|—         |beta |macOS settings in the architecture table    |—                      |pending|—                                                       |
