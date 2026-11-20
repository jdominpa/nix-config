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
|Kernel              |[hardening.nix](../modules/system/core/hardening.nix): boot parameters, sysctls and a module deny list                                                                                                                                                                                    |No hardened kernel, lockdown or module signature enforcement (the NVIDIA modules are unsigned). No AppArmor, SELinux, USBGuard or auditd. The module deny list does not unload loaded modules or stop a privileged insertion. Drives with a blocked filesystem do not mount. The IOMMU only stops DMA attacks while VT-d is on in the firmware                 |
|Administration      |Only `wheel` may use `sudo` and `su`. Root has no password. `sshd` is disabled; if enabled, key-only logins for the user, no root. btrbk runs as root                                                                                                                                     |The user password gives root and is also the disk PIN. Emergency and rescue mode cannot start a shell without a root password: recovery is an older generation or the install media. The btrbk module adds unusable `NOPASSWD` rules for a `btrbk` user                                                                                                        |
|Network             |Firewall on, open ports [below](#network-exposure). Local-only services listen on loopback. Time comes from six NTS-authenticated servers of different operators through chrony, and three must agree                                                                                     |The Steam Remote Play ports are open because Steam is unsandboxed. The live ruleset is not checked automatically. Where outgoing TCP 4460 is blocked, fewer than three servers answer, or the hardware clock is months off, the clock is not synchronised                                                                                                      |
|Supply chain        |Only `wheel` may use the Nix daemon; the user is not in `trusted-users`. Every input is pinned in `flake.lock`. `lanzaboote` and `sops-nix` run as root at every rebuild, `nixpak` with every sandboxed app: review their bumps. Homebrew casks and App Store apps on macOS are not pinned|                                                                                                                                                                                                                                                                                                                                                               |
|Session             |greetd logs the user in once per boot without a password. Noctalia locks the screen after 10 minutes idle and is the polkit agent                                                                                                                                                         |The TPM PIN is the only secret at boot, so the login keyring is unlocked and stored unencrypted on the (encrypted) disk. After a logout the greeter needs a greetd restart or a reboot                                                                                                                                                                         |
|Desktop apps        |Vesktop, Brave, mpv and LocalSend run in nixpak sandboxes ([SANDBOXING.md](SANDBOXING.md))                                                                                                                                                                                                |Everything else reaches the whole home directory. LocalSend's sandbox shares the host network ([sandbox limits](SANDBOXING.md#limits))                                                                                                                                                                                                                         |
|Secrets             |sops-nix ([SECRETS.md](SECRETS.md)). Brave's cookie and password key comes from the Secret portal                                                                                                                                                                                         |User secrets and the age key are readable by any process of the user                                                                                                                                                                                                                                                                                           |
|Backups and recovery|NixOS generations, encrypted backups with restic to a Hetzner Storage Box; btrbk snapshots of `/home` on both local disks ([BACKUPS.md](BACKUPS.md))                                                                                                                                      |A compromised user process can delete the restic repository; only the Storage Box snapshots undo that, within 10 days. The btrbk copies stay on the same machine. A rollback does not undo compromised data                                                                                                                                                    |
|macOS               |Application firewall with stealth mode, guest login and Remote Login off, automatic updates. FileVault and the screen lock delay are set by hand ([README](../README.md))                                                                                                                 |FileVault and the screen lock delay are not enforced by the configuration                                                                                                                                                                                                                                                                                      |

Sources: [hardening](../modules/system/core/hardening.nix),
[secure boot](../modules/system/core/secure-boot.nix),
[disk layout](../modules/hosts/alpha/disko.nix),
[btrfs](../modules/hosts/alpha/btrfs.nix),
[Nix settings](../modules/system/core/nix.nix),
[SSH](../modules/system/core/services/ssh.nix),
[chrony](../modules/system/core/services/chrony.nix),
[restic](../modules/features/restic.nix),
[secrets](../modules/system/core/secrets.nix),
[noctalia](../modules/features/noctalia.nix).

## Network exposure

The firewall is enabled and everything not listed is dropped. The ports below
are opened by the modules named in the last column. Ping is answered.

|Ports                                            |Opened for                   |Source                                  |
|-------------------------------------------------|-----------------------------|----------------------------------------|
|22000/tcp, 22000/udp, 21027/udp                  |Syncthing sync, LAN discovery|`services.syncthing.openDefaultPorts`   |
|53317/tcp, 53317/udp                             |LocalSend (sandboxed)        |`networking.firewall`, desktop-tools.nix|
|27036–27037/tcp, 27031–27036/udp, 10400–10401/udp|Steam Remote Play            |`programs.steam.remotePlay.openFirewall`|

Services meant only for this machine listen on loopback: Syncthing's GUI on
`127.0.0.1:8384` (TLS, requires credentials) and the CoolerControl daemon on
`127.0.0.1:11987` and `[::1]:11987`, which needs a password set by hand to
replace the default one. Syncthing also holds two UDP ports picked at random,
which the firewall does not open. Loopback is not a boundary against the user's
own unsandboxed applications, the ports forwarded into Brave's sandbox
(Syncthing's GUI and CoolerControl) or LocalSend's sandbox, which shares the
host network.

The Bluetooth adapter is off at boot. When it is on, pairable and discoverable
modes end after 30 seconds and it uses private addresses. NetworkManager uses a
new random MAC address on every Wi-Fi and Ethernet connection and IPv6
temporary addresses. The router sees a new device each time, so DHCP
reservations, port forwards and captive portal logins do not persist; a
connection that needs a fixed address can be set to `permanent` with `nmcli`.

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
