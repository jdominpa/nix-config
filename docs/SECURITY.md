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

- User namespaces stay enabled (bubblewrap, nixpak and Chromium's own sandbox
  need them).
- No hardened kernel, no module signature enforcement.

## Architecture and trust boundaries

|Boundary            |Current design                                                                                                                                                                                                                                                                            |Limitation                                                                                                                                                                                                                                                                                                                                                     |
|--------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Data at rest        |LUKS2 (`aes-xts-plain64`, 512-bit key, `argon2id`) on both disks, unlocked in the initrd by a TPM2 keyslot with a PIN bound to PCR 7; the passphrase keyslot is the recovery. The data disk is `nofail`                                                                                   |Keyslots are enrolled by hand. Only PCR 7 is bound: anything the enrolled keys sign unlocks with the PIN, and a PCR 7 change needs the passphrase and a new enrollment ([Boot integrity](#boot-integrity)). TRIM (`allowDiscards`) reveals which blocks are in use. After a wrong PIN the data disk can stay locked; unlock it with `systemd-cryptsetup attach`|
|Boot and firmware   |Lanzaboote signs the bootloader and kernel images with the user's Secure Boot keys; the boot menu editor is disabled; 10 generations are kept. `fwupd` updates the firmware and the dbx. The firmware password is a manual step ([README](../README.md))                                  |Every kept generation boots, older vulnerable kernels included. The signing keys are files readable by root. `sbctl verify` is not run automatically and the firmware password cannot be checked from the OS                                                                                                                                                   |
|Kernel              |The [baseline](#kernel-and-process-baseline) below                                                                                                                                                                                                                                        |No hardened kernel, lockdown or module signature enforcement (the NVIDIA modules are unsigned). No AppArmor, SELinux, USBGuard or auditd. The IOMMU only stops DMA attacks if VT-d is on in the firmware (not checked, [Audits](#audits))                                                                                                                      |
|Administration      |Only `wheel` may use `sudo` and `su`. `sshd` is disabled; if enabled, key-only logins for the user, no root. btrbk runs as root                                                                                                                                                           |The user password gives root and is also the disk PIN. The btrbk module adds unusable `NOPASSWD` rules for a `btrbk` user                                                                                                                                                                                                                                      |
|Network             |Firewall on, open ports [below](#network-exposure). Local-only services listen on loopback                                                                                                                                                                                                |The Steam Remote Play ports are open because Steam is unsandboxed. The live ruleset is not checked automatically                                                                                                                                                                                                                                               |
|Supply chain        |Only `wheel` may use the Nix daemon; the user is not in `trusted-users`. Every input is pinned in `flake.lock`. `lanzaboote` and `sops-nix` run as root at every rebuild, `nixpak` with every sandboxed app: review their bumps. Homebrew casks and App Store apps on macOS are not pinned|                                                                                                                                                                                                                                                                                                                                                               |
|Session             |greetd logs the user in once per boot without a password. noctalia locks the screen after 10 minutes idle and is the polkit agent                                                                                                                                                         |The TPM PIN is the only secret at boot. After a logout the greeter needs a greetd restart or a reboot                                                                                                                                                                                                                                                          |
|Desktop apps        |Vesktop, Brave, mpv and LocalSend run in nixpak sandboxes ([SANDBOXING.md](SANDBOXING.md))                                                                                                                                                                                                |Everything else, Steam, Emacs, Bitwarden and the terminal included, reaches the whole home directory. LocalSend's sandbox shares the host network and is not checked yet ([sandbox limits](SANDBOXING.md#limits))                                                                                                                                              |
|Secrets             |sops-nix ([SECRETS.md](SECRETS.md)). Brave's cookie and password key comes from the Secret portal                                                                                                                                                                                         |User secrets and the age key are readable by any process of the user                                                                                                                                                                                                                                                                                           |
|Backups and recovery|NixOS generations, encrypted backups with restic to a Hetzner Storage Box; btrbk snapshots of `/home` on both local disks ([BACKUPS.md](BACKUPS.md))                                                                                                                                      |A compromised user process can delete the restic repository; only the Storage Box snapshots undo that, within 10 days. The btrbk copies stay on the same machine. A rollback does not undo compromised data                                                                                                                                                    |
|macOS               |Application firewall with stealth mode, guest login and Remote Login off, automatic updates. FileVault and the screen lock delay are set by hand ([README](../README.md))                                                                                                                 |Checked ([Audits](#audits)). Automatic updates are on but stalled: 26.6.2 runs while 26.7 to 26.7.3 have been offered since September                                                                                                                                                                                                                          |

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
configuration and in the running ruleset: [Audits](#audits)). Ping is answered.

|Ports                                            |Opened for                   |Source                                  |
|-------------------------------------------------|-----------------------------|----------------------------------------|
|22000/tcp, 22000/udp, 21027/udp                  |Syncthing sync, LAN discovery|`services.syncthing.openDefaultPorts`   |
|53317/tcp, 53317/udp                             |LocalSend (sandboxed)        |`networking.firewall`, desktop-tools.nix|
|27036–27037/tcp, 27031–27036/udp, 10400–10401/udp|Steam Remote Play            |`programs.steam.remotePlay.openFirewall`|

Listening sockets besides the ports above, as last observed ([Audits](#audits)):
Syncthing's GUI on `127.0.0.1:8384` (TLS, requires credentials) and the
CoolerControl daemon on `127.0.0.1:11987` and `[::1]:11987`, which needs a
password and refuses the default one (checked). Syncthing also holds two UDP
ports picked at random, which the firewall does not open. Loopback is not a
boundary against the user's own unsandboxed applications, the ports forwarded
into Brave's sandbox (Syncthing's GUI and CoolerControl) or LocalSend's sandbox,
which shares the host network.

The Bluetooth adapter is off at boot (`hardware.bluetooth.powerOnBoot`). When it
is on, pairable and discoverable modes end after 30 seconds and it uses private
addresses. NetworkManager uses a new random MAC address on every Wi-Fi
connection and IPv6 temporary addresses (not checked). The router sees a new
device each time, so DHCP reservations, port forwards and captive portal logins
do not persist; a network that needs a fixed address can be set to `permanent`
with `nmcli`.

## Kernel and process baseline

[hardening.nix](../modules/system/core/hardening.nix) holds the settings, each
with a comment on what it does and what it costs. It sets kernel boot parameters
(allocator hardening, memory zeroed on allocation and on free, randomisation, no
vsyscall page, no debugfs, hardened usercopy, ptrace rules for writes to
`/proc/<pid>/mem`, a forced strict IOMMU, no writes to the block devices of
mounted filesystems), sysctls (kernel pointers and log hidden, BPF for root
only, io_uring off, restricted ptrace and perf, no core dumps of setuid
programs, maximum `mmap` ASLR, link and FIFO protections, no ICMP redirects or
source routing), disables kexec and hibernation, forces page table isolation,
does not store core dumps and blocks a list of kernel modules. Some values are
already kernel, systemd or NixOS defaults; setting them explicitly makes the
desired baseline reviewable.

What you notice in daily use:

- `gdb -p` and `strace -p` on an unrelated process need sudo.
- No kernel profiling for unprivileged users, but user-space profiling still
  works.
- `/sys/kernel/debug` is absent and tools that need it, such as powertop,
  complain.
- A crash is logged in the journal, but `coredumpctl debug` has no dump to open.
- No hibernation.
- Programs that need io_uring fail or fall back to other I/O.
- `iommu.strict=1` costs some I/O speed (not measured).

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
ls /sys/class/iommu  # empty means the IOMMU is off

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
for m in /boot /home /tmp /.snapshots /data/games /data/backup; do
  findmnt -n -o TARGET,OPTIONS "$m"
done

# Backup
systemctl status restic-backups-storagebox.timer
journalctl -u restic-backups-storagebox.service -n 50
restic-storagebox snapshots
```

## Audits

Results of the checks above; those of the secrets, sandboxes and backups are in
their own documents. Each holds for the configuration it ran against.

To check, on `alpha` after switching to the additions of 2026-11-19:

- Boot parameters: `/proc/cmdline` against `nix eval` of `boot.kernelParams`.
- Sysctls: `kernel.io_uring_disabled` is 2 and `fs.suid_dumpable` is 0.
- IOMMU: `/sys/class/iommu` lists a `dmar` device and `journalctl -k -g DMAR`
  shows it enabled (the directory was empty on 2026-11-18; if it still is, turn
  on VT-d in the firmware). The GPU, NVMe disks and Wi-Fi still work. Measure the
  cost of `iommu.strict=1` with the `hyperfine` build of the `init_on_free` row.
- `su`: `/etc/pam.d/su` has `pam_wheel`.
- Bluetooth: `bluetoothctl show` says `Powered: no` after boot;
  `/etc/bluetooth/main.conf` has `AutoEnable=false`, both timeouts and
  `Privacy=device`; paired devices still connect once it is turned on.
- Wi-Fi: `nmcli -f GENERAL.HWADDR device show wlo1` differs from
  `ethtool -P wlo1`; `ip -6 addr show wlo1` has a `temporary` address if the
  network has IPv6.

|Date      |Host |Check                                                             |How                                                                                                                                                                                    |Findings                                                                                                                                                                                                                   |
|----------|-----|------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|2026-11-13|alpha|Listening sockets besides the firewall ports                      |Not recorded                                                                                                                                                                           |Only the loopback services listed under Network exposure                                                                                                                                                                   |
|2026-11-16|alpha|Open ports in the evaluated configuration                         |`nix eval` of `networking.firewall.allowed{TCP,UDP}Ports` and `allowedUDPPortRanges`, run on `beta`                                                                                    |The ports in the Network exposure table and no others                                                                                                                                                                      |
|2026-11-16|alpha|Boot menu editor in the evaluated configuration                   |`nix eval` of `boot.lanzaboote.settings`, run on `beta`                                                                                                                                |`editor` is `false`                                                                                                                                                                                                        |
|2026-11-16|alpha|Recovery passphrase of the system disk, before the TPM2 enrollment|`cryptsetup open --test-passphrase --disable-external-tokens`                                                                                                                          |Accepted                                                                                                                                                                                                                   |
|2026-11-16|alpha|TPM2 unlock of both disks                                         |Boot journal of the 16:49 boot                                                                                                                                                         |One PIN prompt; `crypted` and `cryptdata` unlocked, no passphrase asked                                                                                                                                                    |
|2026-11-16|alpha|Wrong PIN at boot                                                 |Boot journal of the 18:26 boot                                                                                                                                                         |The system disk asked again and unlocked; the data disk stayed locked, its prompt was dropped at the switch to the real root, `/data/*` were not mounted (`nofail`)                                                        |
|2026-11-16|alpha|Auto-login                                                        |`loginctl list-sessions` and the user's report, 18:26 boot                                                                                                                             |Session on `tty1` without the greeter                                                                                                                                                                                      |
|2026-11-16|alpha|Login keyring after auto-login                                    |`busctl --user get-property` of `Locked` on the default collection; header of `login.keyring`                                                                                          |Unlocked; the file is in the unencrypted `[keyring]` format                                                                                                                                                                |
|2026-11-16|alpha|Brave's secrets after auto-login                                  |`os_crypt.portal.prev_init_success` in `Local State`                                                                                                                                   |`true`, written in the auto-login session                                                                                                                                                                                  |
|2026-11-16|alpha|Secure Boot state                                                 |`bootctl status`                                                                                                                                                                       |`enabled (user)`; booted through `lanzastub` and `systemd-boot`                                                                                                                                                            |
|2026-11-16|alpha|Disk cipher                                                       |Journal of `systemd-cryptsetup` in the 16:27 boot                                                                                                                                      |`aes`, `xts-plain64`, 512-bit key on both disks                                                                                                                                                                            |
|2026-11-16|alpha|Mount options                                                     |`findmnt` on each mount point                                                                                                                                                          |As in the Mounts row, on all six mounts; `/boot` has `fmask=0177,dmask=0077`                                                                                                                                               |
|2026-11-16|alpha|Boot parameters                                                   |`/proc/cmdline` against `nix eval` of `boot.kernelParams`                                                                                                                              |Identical                                                                                                                                                                                                                  |
|2026-11-16|alpha|Sysctls                                                           |`/proc/sys` against `nix eval` of `boot.kernel.sysctl`                                                                                                                                 |All match; `net.core.bpf_jit_harden`, `vm.mmap_rnd_bits` and `vm.mmap_rnd_compat_bits` cannot be read without root                                                                                                         |
|2026-11-16|alpha|Lockdown state                                                    |`cat /sys/kernel/security/lockdown`                                                                                                                                                    |`[none]`: off                                                                                                                                                                                                              |
|2026-11-16|alpha|Listening sockets on the new install                              |`ss -tuln -e`; owners from the cgroup (UDP) or the known port (TCP)                                                                                                                    |TCP: Syncthing on `*:22000` and `127.0.0.1:8384`, CUPS on `127.0.0.1:631`, CoolerControl on `127.0.0.1:11987` and `[::1]:11987`. UDP: Syncthing on 22000, 21027 and two random ports, Brave's mDNS on `224.0.0.251:5353`   |
|2026-11-16|alpha|CoolerControl default password                                    |`POST /login` with the default credentials, before and after setting a password                                                                                                        |Accepted before (200); refused after (401), as is a request without credentials                                                                                                                                            |
|2026-11-16|alpha|Live firewall ruleset                                             |`sudo nft list ruleset`                                                                                                                                                                |Input accepts loopback, established connections, the ports of the Network exposure table and no others, ping, ICMPv6 and DHCPv6 replies; everything else is dropped. Same rules for IPv4 and IPv6, through `iptables-nft`  |
|2026-11-16|alpha|Signatures of the boot files                                      |`sudo sbctl verify`                                                                                                                                                                    |`BOOTX64.EFI`, `systemd-bootx64.efi` and the five generations are signed. The kernel in `EFI/nixos` is not, as Lanzaboote intends: the signed stub of each generation carries its hash                                     |
|2026-11-16|alpha|LUKS keyslots of both disks                                       |`sudo cryptsetup luksDump` on each                                                                                                                                                     |Keyslot 0: `argon2id`, 1 GiB, 512-bit key (the passphrase). Keyslot 1: held by the `systemd-tpm2` token. `aes-xts-plain64`, `allow-discards`. No other keyslots                                                            |
|2026-11-16|alpha|Recovery passphrase of both disks after the TPM2 enrollment       |`sudo cryptsetup open --test-passphrase --disable-external-tokens` on each, by the user                                                                                                |Accepted on both                                                                                                                                                                                                           |
|2026-11-16|alpha|Sysctls only root can read                                        |`sudo sysctl net.core.bpf_jit_harden vm.mmap_rnd_bits vm.mmap_rnd_compat_bits`, by the user                                                                                            |The configured values                                                                                                                                                                                                      |
|2026-11-16|alpha|PCR binding and PIN of the TPM2 keyslots                          |`sudo cryptsetup luksDump --dump-json-metadata` on each disk, by the user                                                                                                              |`"tpm2-pcrs":[7]` and `"tpm2-pin":true`                                                                                                                                                                                    |
|2026-11-17|alpha|Core dumps not stored                                             |`systemd-analyze cat-config systemd/coredump.conf`; `kill -SEGV` of a `sleep`, then `coredumpctl list` and `ls /var/lib/systemd/coredump`                                              |`Storage=none`; the crash is listed with `COREFILE` `none` and the directory is empty. The journal still gets the stack trace (addresses and symbols, no memory contents)                                                  |
|2026-11-17|alpha|Cost of `init_on_free=1`                                          |`hyperfine --warmup 1 --runs 5 'nix build nixpkgs#ripgrep --rebuild --no-link'` and `sysbench memory --memory-block-size=1M --memory-total-size=20G run`, before and after, by the user|Build 24.455 ± 0.173 s before, 24.622 ± 0.193 s after (+0.7%, within the noise); memory write speed 29177 and 29228 MiB/s                                                                                                  |
|2026-11-17|alpha|Boot parameters with `init_on_free=1`                             |`/proc/cmdline` against `nix eval` of `boot.kernelParams`                                                                                                                              |Identical                                                                                                                                                                                                                  |
|2026-11-17|beta |Application firewall                                              |`socketfilterfw --getglobalstate --getstealthmode --getallowsigned`                                                                                                                    |On, stealth mode on, built-in signed software allowed, downloaded signed software not                                                                                                                                      |
|2026-11-17|beta |FileVault and screen lock delay                                   |`fdesetup status`; `sysadminctl -screenLock status`                                                                                                                                    |`FileVault is On.`; `screenLock delay is immediate`                                                                                                                                                                        |
|2026-11-17|beta |Automatic macOS updates                                           |`defaults read /Library/Preferences/com.apple.SoftwareUpdate`; `sw_vers`                                                                                                               |`AutomaticallyInstallMacOSUpdates`, `AutomaticDownload` and `CriticalUpdateInstall` are `1`, but the last install was 26.6.2 on 2026-09-03; 26.7.3 (offered 2026-11-10) and Safari 27.2 are pending. Cause not investigated|
|2026-11-17|alpha|Open ports in the evaluated configuration, LocalSend sandboxed     |`nix eval` of `networking.firewall.allowed{TCP,UDP}Ports` and `allowedUDPPortRanges`, run on `alpha` before the switch                                                                 |The ports in the Network exposure table and no others; 53317 now comes from `desktop-tools.nix`. The live ruleset is not rechecked                                                                                       |
