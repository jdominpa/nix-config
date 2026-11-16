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

|Boundary            |Current design                                                                                                                                                                                                                                                                            |Limitation                                                                                                                                                                                                                                                                                                                                                     |
|--------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Data at rest        |LUKS2 (`aes-xts-plain64`, 512-bit key, `argon2id`) on both disks, unlocked in the initrd by a TPM2 keyslot with a PIN bound to PCR 7; the passphrase keyslot is the recovery. The data disk is `nofail`                                                                                   |Keyslots are enrolled by hand. Only PCR 7 is bound: anything the enrolled keys sign unlocks with the PIN, and a PCR 7 change needs the passphrase and a new enrollment ([Boot integrity](#boot-integrity)). TRIM (`allowDiscards`) reveals which blocks are in use. After a wrong PIN the data disk can stay locked; unlock it with `systemd-cryptsetup attach`|
|Boot and firmware   |Lanzaboote signs the bootloader and kernel images with the user's Secure Boot keys; the boot menu editor is disabled; 10 generations are kept. `fwupd` updates the firmware and the dbx. The firmware password is a manual step ([README](../README.md))                                  |Every kept generation boots, older vulnerable kernels included. The signing keys are files readable by root. `sbctl verify` is not run automatically and the firmware password cannot be checked from the OS                                                                                                                                                   |
|Kernel              |The [baseline](#kernel-and-process-baseline) below                                                                                                                                                                                                                                        |No hardened kernel, lockdown or module signature enforcement (the NVIDIA modules are unsigned). No AppArmor, SELinux, USBGuard or auditd                                                                                                                                                                                                                       |
|Administration      |Only `wheel` may use `sudo`. `sshd` is disabled; if enabled, key-only logins for the user, no root. btrbk runs as root                                                                                                                                                                    |The user password gives root and is also the disk PIN. The btrbk module adds unusable `NOPASSWD` rules for a `btrbk` user                                                                                                                                                                                                                                      |
|Network             |Firewall on, open ports [below](#network-exposure). Local-only services listen on loopback                                                                                                                                                                                                |The Steam Remote Play ports are open because Steam is unsandboxed. The live ruleset is not checked automatically                                                                                                                                                                                                                                               |
|Supply chain        |Only `wheel` may use the Nix daemon; the user is not in `trusted-users`. Every input is pinned in `flake.lock`. `lanzaboote` and `sops-nix` run as root at every rebuild, `nixpak` with every sandboxed app: review their bumps. Homebrew casks and App Store apps on macOS are not pinned|                                                                                                                                                                                                                                                                                                                                                               |
|Session             |greetd logs the user in once per boot without a password. noctalia locks the screen after 10 minutes idle and is the polkit agent                                                                                                                                                         |The TPM PIN is the only secret at boot. After a logout the greeter needs a greetd restart or a reboot                                                                                                                                                                                                                                                          |
|Desktop apps        |Vesktop, Brave and mpv run in nixpak sandboxes ([SANDBOXING.md](SANDBOXING.md))                                                                                                                                                                                                           |Everything else, Steam, Emacs, Bitwarden and the terminal included, reaches the whole home directory ([sandbox limits](SANDBOXING.md#limits))                                                                                                                                                                                                                  |
|Secrets             |sops-nix ([SECRETS.md](SECRETS.md)). Brave's cookie and password key comes from the Secret portal                                                                                                                                                                                         |User secrets and the age key are readable by any process of the user                                                                                                                                                                                                                                                                                           |
|Backups and recovery|NixOS generations, encrypted backups with restic to a Hetzner Storage Box; btrbk snapshots of `/home` on both local disks ([BACKUPS.md](BACKUPS.md))                                                                                                                                      |A compromised user process can delete the restic repository; only the Storage Box snapshots undo that, within 10 days. The btrbk copies stay on the same machine. A rollback does not undo compromised data                                                                                                                                                    |
|macOS               |Application firewall with stealth mode, guest login and Remote Login off, automatic updates. FileVault and the screen lock delay are set by hand ([README](../README.md))                                                                                                                 |Not checked on the machine                                                                                                                                                                                                                                                                                                                                     |

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

Listening sockets besides the ports above, as last observed ([Audits](#audits)):
Syncthing's GUI on `127.0.0.1:8384` (TLS, requires credentials), CUPS on
`127.0.0.1:631` and `[::1]:631`, and the CoolerControl daemon on
`127.0.0.1:11987` and `[::1]:11987` (answers 401 without credentials; whether
its default password was changed is not checked). Loopback is not a boundary
against the user's own unsandboxed applications or against sandboxes that share
the host network (Brave).

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

The TPM2 releases the disk key only with the PIN and an unchanged PCR 7, which
measures the Secure Boot state and keys. PCR 7 changes with a dbx update from
`fwupd`, new `sbctl` keys or toggling Secure Boot. The boot then asks for the
passphrase, and the TPM2 keyslot must be enrolled again on `disk-system-luks`
and `disk-data-luks`:

```sh
sudo systemd-cryptenroll --wipe-slot=tpm2 --tpm2-device=auto \
  --tpm2-with-pin=yes --tpm2-pcrs=7 /dev/disk/by-partlabel/<label>
```

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

# Disk keyslots (expected: one password and one tpm2 slot per disk)
sudo systemd-cryptenroll /dev/disk/by-partlabel/disk-system-luks
sudo systemd-cryptenroll /dev/disk/by-partlabel/disk-data-luks

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
- `alpha`: TPM2 enrollment of both disks (`sudo cryptsetup luksDump` on each);
  expected: one password keyslot and a `systemd-tpm2` token with `tpm2-pcrs: 7`
  and `tpm2-pin: true`.
- `alpha`: the recovery passphrase of both disks after the TPM2 enrollment
  (`sudo cryptsetup open --test-passphrase --disable-external-tokens` on each).
- `beta`: the macOS settings in the architecture table.

|Date      |Host |Check                                                             |How                                                                                                |Findings                                                                                                                                                           |
|----------|-----|------------------------------------------------------------------|---------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|2026-11-13|alpha|Listening sockets besides the firewall ports                      |Not recorded                                                                                       |Only the loopback services listed under Network exposure                                                                                                           |
|2026-11-16|alpha|Open ports in the evaluated configuration                         |`nix eval` of `networking.firewall.allowed{TCP,UDP}Ports` and `allowedUDPPortRanges`, run on `beta`|The ports in the Network exposure table and no others                                                                                                              |
|2026-11-16|alpha|Boot menu editor in the evaluated configuration                   |`nix eval` of `boot.lanzaboote.settings`, run on `beta`                                            |`editor` is `false`                                                                                                                                                |
|2026-11-16|alpha|Recovery passphrase of the system disk, before the TPM2 enrollment|`cryptsetup open --test-passphrase --disable-external-tokens`                                      |Accepted                                                                                                                                                           |
|2026-11-16|alpha|TPM2 unlock of both disks                                         |Boot journal of the 16:49 boot                                                                     |One PIN prompt; `crypted` and `cryptdata` unlocked, no passphrase asked                                                                                            |
|2026-11-16|alpha|Wrong PIN at boot                                                 |Boot journal of the 18:26 boot                                                                     |The system disk asked again and unlocked; the data disk stayed locked, its prompt was dropped at the switch to the real root, `/data/*` were not mounted (`nofail`)|
|2026-11-16|alpha|Auto-login                                                        |`loginctl list-sessions` and the user's report, 18:26 boot                                         |Session on `tty1` without the greeter                                                                                                                              |
|2026-11-16|alpha|Login keyring after auto-login                                    |`busctl --user get-property` of `Locked` on the default collection; header of `login.keyring`      |Unlocked; the file is in the unencrypted `[keyring]` format                                                                                                        |
|2026-11-16|alpha|Brave's secrets after auto-login                                  |`os_crypt.portal.prev_init_success` in `Local State`                                               |`true`, written in the auto-login session                                                                                                                          |
