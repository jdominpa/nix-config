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
|Data at rest  |LUKS2 over the whole system partition (`aes-xts-plain64`, 512-bit key, `argon2id`, 5 s), with btrfs subvolumes `@`, `@nix`, `@home`, `@snapshots` and `@swap` inside, so swap is encrypted too. Unlocked by passphrase in the initrd. `allowDiscards` is on: TRIM reveals which blocks are in use. On `alpha`, the data disk (`@games`, `@backup`) has its own LUKS2 with the same settings and passphrase, unlocked in the initrd with the same prompt; it is `nofail`, so boot does not need it|
|Mounts        |`/boot` is not encrypted; its contents are signed, and it is mounted `noexec,nosuid,nodev` with `0600`/`0700` permissions. `/home` is `nosuid,nodev`, `/.snapshots` and `/data/backup` `noexec,nosuid,nodev`, `/data/games` `nosuid,nodev`, `/tmp` a tmpfs emptied at boot                                                                                         |
|Boot chain    |Lanzaboote signs the bootloader and the unified kernel images; Secure Boot is enabled with the keys of the user (`/var/lib/sbctl`). The boot menu editor is disabled, so `init=/bin/sh` cannot be appended at the menu. Ten generations are kept (`configurationLimit`)                                           |
|Firmware      |`fwupd` is enabled, which also delivers Secure Boot revocation list (dbx) updates. The firmware administrator password is a manual step ([README](../README.md)) and cannot be checked from the OS                                                                                                                |
|Kernel        |The [baseline](#kernel-and-process-baseline) below                                                                                                                                                                                                                                                                |
|Administration|Only `wheel` may execute `sudo`. The btrbk module adds `NOPASSWD` rules for a `btrbk` user, unusable since it is not in `wheel`; btrbk runs as root instead. The user is in `wheel` and `networkmanager`. `sshd` is disabled on both hosts; if it is enabled, the settings in `hardening.nix` allow key-only logins for the one user, no root login                                                                                           |
|Network       |Host firewall on; the open ports are listed below. Services that only need the machine itself listen on loopback                                                                                                                                                                                                  |
|Nix           |Only `wheel` may use the Nix daemon (`allowed-users`); `trusted-users` is the default (root). Every flake input is pinned in `flake.lock`. Extra binary cache: `nix-community.cachix.org`, with its key pinned. Unfree packages are allowed                                                                       |
|Session       |`noctalia-greeter` as login manager. noctalia locks the screen after 10 minutes of idle time and turns the screens off after 11; it also provides the polkit agent                                                                                                                                                |
|Desktop apps  |Vesktop, Brave and mpv run in nixpak sandboxes ([SANDBOXING.md](SANDBOXING.md)); everything else runs with the full access of the user                                                                                                                                                                            |
|Secrets       |sops-nix; user secrets and the age key are readable by any process of the user ([SECRETS.md](SECRETS.md)). Brave's cookie and password key comes from the Secret portal                                                                                                                                           |
|Backups       |restic to a Hetzner Storage Box, encrypted on the client, and btrbk snapshots of `/home` on both local disks ([BACKUPS.md](BACKUPS.md))                                                                                                                                                                                                                               |
|Recovery      |NixOS generations (10 in the boot menu; weekly garbage collection of generations older than 7 days, so about a week of rollback), btrbk and restic snapshots                                                                                                                                                            |
|macOS         |Application firewall on with stealth mode, signed system services allowed, guest login off, automatic macOS updates, Remote Login off. FileVault and the screen lock delay are manual steps ([README](../README.md)); none of this was checked on the machine                                                     |

Sources: [hardening](../modules/system/core/hardening.nix),
[secure boot](../modules/system/core/secure-boot.nix),
[disk layout](../modules/hosts/alpha/disko.nix),
[btrfs](../modules/hosts/alpha/btrfs.nix),
[Nix settings](../modules/system/core/nix.nix),
[SSH](../modules/system/core/services/ssh.nix),
[restic](../modules/features/restic.nix),
[secrets](../modules/system/core/secrets.nix),
[noctalia](../modules/features/noctalia.nix).

## Network exposure

The firewall is enabled and everything not listed is dropped. The ports below
are opened by the modules named in the last column (checked in the evaluated
configuration, not in the running ruleset: [Audits](#audits)).

|Ports                                            |Opened for                   |Source                                  |
|-------------------------------------------------|-----------------------------|----------------------------------------|
|22000/tcp, 22000/udp, 21027/udp                  |Syncthing sync, LAN discovery|`services.syncthing.openDefaultPorts`   |
|53317/tcp, 53317/udp                             |LocalSend                    |`programs.localsend`                    |
|27036–27037/tcp, 27031–27036/udp, 10400–10401/udp|Steam Remote Play            |`programs.steam.remotePlay.openFirewall`|

Listening sockets besides the ports above, as last observed
([Audits](#audits)): Syncthing's GUI on `127.0.0.1:8384` (TLS, requires
credentials), CUPS on `127.0.0.1:631` and `[::1]:631`, and the CoolerControl
daemon on `127.0.0.1:11987` and `[::1]:11987` (answers 401 without credentials;
whether its default password was changed is not checked).
Loopback is not a boundary against the user's own unsandboxed applications or
against sandboxes that share the host network (Brave).

Bluetooth is powered on at boot (`hardware.bluetooth.powerOnBoot`) and CUPS is
enabled; neither is restricted further.

## Kernel and process baseline

[hardening.nix](../modules/system/core/hardening.nix) holds the settings, each
with a comment on what it does and what it costs. It sets kernel boot parameters
(allocator hardening, randomisation, no vsyscall page, no debugfs), sysctls
(kernel pointers and log hidden, BPF for root only, restricted ptrace and perf,
maximum `mmap` ASLR, link and FIFO protections, no ICMP redirects or source
routing), disables kexec and hibernation, forces page table isolation and blocks
a list of kernel modules. Some values are already kernel, systemd or NixOS
defaults; setting them explicitly makes the desired baseline reviewable. The
values on the running system are not checked ([Audits](#audits)).

What you notice in daily use:

- `gdb -p` and `strace -p` on an unrelated process need sudo; debugging a
  program you launch is unaffected.
- No kernel profiling for unprivileged users, but user-space profiling still
  works.
- `/sys/kernel/debug` is absent and tools that need it, such as powertop,
  complain.
- No hibernation.

The module deny list covers obscure network protocols, three filesystems and
the video test driver `vivid`; `esp4`, `esp6` and `rxrpc` are on it as WA-004
([WORKAROUNDS.md](WORKAROUNDS.md)). The modules are blocked with
`install <module> false`, which also stops an explicit `modprobe`, so an
unprivileged `socket()` or `mount` cannot auto-load them. The list does not
unload modules that are already loaded, does not affect built-in code and does
not stop a privileged module insertion.

## Boot integrity

- Kernel lockdown is **off**: `/sys/kernel/security/lockdown` read `[none]
  integrity confidentiality` (observed, date not recorded: [Audits](#audits)).
  Module signature enforcement is not configured either; the NVIDIA modules
  would have to be signed first.
- A TPM 2.0 is present, but nothing uses it: no measured boot and no TPM-bound
  disk unlock. The disk always needs the passphrase.

## Supply chain

- `flake.lock` pins every input. Updates are a deliberate `just update`.
- `nixpkgs` follows `nixos-unstable`: fixes arrive quickly, so do regressions.
- The extra substituter `nix-community.cachix.org` is trusted through the option
  `nix.settings`.
- `lanzaboote` runs as root at every rebuild and signs the boot files with the
  Secure Boot keys: its bumps are the most security-relevant in a `flake.lock`
  diff.
- `nixpak` is a flake input and runs as the user on every launch of a sandboxed
  application: treat bumps of it, `xdg-dbus-proxy` and `bubblewrap` as
  security-relevant when reading a `flake.lock` diff.
- `sops-nix` runs as root at every activation to decrypt the secrets: treat its
  bumps the same way. The `secrets` input is a private repository fetched over
  SSH. It is read as plain files (`flake = false`), so it adds ciphertext to the
  evaluation, not code.
- On macOS, Homebrew casks and Mac App Store applications (Syncthing,
  Bitwarden, LocalSend) are outside `flake.lock`: they are not pinned and
  update on their own schedule.

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
findmnt /boot /home /tmp /.snapshots /data/games /data/backup

# Backup
systemctl status restic-backups-storagebox.timer
journalctl -u restic-backups-storagebox.service -n 50
restic-storagebox snapshots
```

## Audits

Results of the checks above; those of the secrets, sandboxes and backups are in
their own documents. Each holds for the configuration it ran against.

To check:

- `alpha`: live firewall ruleset against the port table (`sudo nft list
  ruleset`).
- `alpha`: Secure Boot and signatures on the new disk (`bootctl status`, `sudo
  sbctl verify`).
- `alpha`: LUKS parameters of the new disk (`sudo cryptsetup luksDump`);
  expected: a 512-bit key and `argon2id`.
- `alpha`: mount options (`findmnt /boot /home /.snapshots /tmp`).
- `alpha`: boot parameters and sysctls (`cat /proc/cmdline`, the `sysctl`
  command above).
- `alpha`: lockdown state (`cat /sys/kernel/security/lockdown`); `[none]` was
  seen before, without a recorded date.
- `alpha`: listening sockets again, on the new install (`ss -tulpn`).
- `alpha`: the CoolerControl password is not the default one.
- `beta`: the macOS settings in the architecture table.

|Date      |Host |Check                                          |How                                                                                                |Findings                                                |
|----------|-----|-----------------------------------------------|---------------------------------------------------------------------------------------------------|--------------------------------------------------------|
|2026-11-13|alpha|Listening sockets besides the firewall ports   |Not recorded                                                                                       |Only the loopback services listed under Network exposure|
|2026-11-16|alpha|Open ports in the evaluated configuration      |`nix eval` of `networking.firewall.allowed{TCP,UDP}Ports` and `allowedUDPPortRanges`, run on `beta`|The ports in the Network exposure table and no others   |
|2026-11-16|alpha|Boot menu editor in the evaluated configuration|`nix eval` of `boot.lanzaboote.settings`, run on `beta`                                            |`editor` is `false`                                     |
