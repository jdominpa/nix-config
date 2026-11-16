This repository contains the [Nix](https://nix.dev/) code that builds all my
systems:

1. **NixOS**: [niri](https://github.com/niri-wm/niri),
   [Noctalia](https://github.com/noctalia-dev/noctalia) shell.
2. **macOS**: [nix-darwin](https://github.com/nix-darwin/nix-darwin).

See [./docs](./docs/README.md) for the documentation index,
[./modules/hosts](./modules/hosts) for the host list, and
[./AGENTS.md](./AGENTS.md) for the repository conventions.

## Deployment

### NixOS

These steps assume the host already exists in the flake (see [How to add a new
host](#how-to-add-a-new-host)).

#### 1. Boot the installer

Download the NixOS ISO image from the NixOS [download
page](https://nixos.org/download.html#nixos-iso) and create a bootable USB drive
following the instructions in [Section 2.4.1 "Booting from a USB flash
drive"](https://nixos.org/manual/nixos/stable/index.html#sec-booting-from-usb)
of the NixOS manual. Then, boot the machine from the USB drive.

#### 2. Format the disk

```bash
# Clone this flake
git clone https://github.com/jdominpa/nix-config /tmp/nix-config
```

The installer cannot fetch the private secrets repository: bring a clone to
`/tmp/nix-secrets` (copied, or cloned over HTTPS with a GitHub token) and lock
the flake to it. The age key is restored after the first login
([SECRETS.md](docs/SECRETS.md#adding-a-new-host)).

```bash
nix --experimental-features "nix-command flakes" flake lock /tmp/nix-config \
  --override-input secrets path:/tmp/nix-secrets
```

Identify the system disk with `ls -l /dev/disk/by-id/` and set `device` in the
host's `disko.nix` to the disk's `/dev/disk/by-id/` path. Then, format it:

```bash
sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko/latest -- \
  --mode destroy,format,mount \
  --flake /tmp/nix-config#<hostname>
```

This formats every disk in the host's `disko.nix`. To format one disk only, use
`#<hostname>-<disk>`.

#### 3. Generate hardware configuration

Generate it without file systems (disko manages those) and copy it into the
host's `hardware.nix`:

```bash
sudo nixos-generate-config --no-filesystems --root /mnt
```

#### 4. Install NixOS

Since every NixOS host uses Secure Boot and signs its boot files during the
installation, the signing keys have to be in place before `nixos-install` runs.

```bash
# Create the Secure Boot keys and copy them to the new system
sudo nix --experimental-features "nix-command flakes" run nixpkgs#sbctl -- create-keys
sudo mkdir -p /mnt/var/lib
sudo cp -a /var/lib/sbctl /mnt/var/lib/

# Install NixOS with <hostname> configuration
sudo nixos-install --flake /tmp/nix-config#<hostname> --root /mnt

# Set user password for any users defined in the configuration
sudo nixos-enter --root /mnt -c 'passwd <username>'

# Undo the lock file change from step 2, then copy the flake (with the
# hardware.nix changes) to the machine's filesystem
git -C /tmp/nix-config checkout flake.lock
sudo cp -r /tmp/nix-config /mnt/etc/nixos/nix-config

# Unmount and reboot
sudo umount -R /mnt
reboot
```

#### 5. Enable Secure Boot

Secure Boot is provided by
[lanzaboote](https://github.com/nix-community/lanzaboote), which the
`secure-boot` module sets up in place of plain systemd-boot. After Step 4 the
boot files are signed, but the firmware does not know the keys yet.

1. Check that the boot files are signed. Only the kernel images under
   `/boot/EFI/nixos` are expected to be reported as not signed.

   ```bash
   sudo sbctl verify
   ```

2. Reboot into the firmware setup, enable Secure Boot and put it into Setup
   Mode.

   ```bash
   systemctl reboot --firmware-setup
   ```

3. Boot NixOS again and enroll the keys. `--microsoft` keeps the certificates of
   Microsoft next to them since the firmware of the graphics card is usually
   signed with those, and without them the machine may not boot anymore.

   ```bash
   sudo sbctl enroll-keys --microsoft
   ```

4. Reboot and check that Secure Boot is active. If it is not, enable it in the
   firmware setup. Some boards also need "OS Type" set to "Windows UEFI Mode".

   ```bash
   bootctl status | grep "Secure Boot" # Secure Boot: enabled (user)
   ```

5. Set an administrator password in the firmware setup, otherwise anyone at the
   keyboard can turn Secure Boot off again.

From here on every rebuild signs the new generation by itself.

### macOS

For first time deployment, install
[nix](https://nixos.org/download/#nix-install-macos) and
[homebrew](https://brew.sh) manually. Then proceed as follows:

Restore the personal age key to
`~/Library/Application Support/sops/age/keys.txt` before the first switch,
otherwise the secrets are not decrypted
([SECRETS.md](docs/SECRETS.md#adding-a-new-host)).

```bash
# Prepare the deployment environment
nix-shell -p git just

# Deploy using `just` & Justfile
just switch <hostname>
```

Some settings cannot be set by nix-darwin because macOS only changes them after
asking for the password of the user, so they have to be set once by hand.

```bash
# Encrypt the disk with FileVault. This prints a recovery key: store it in the
# password manager, it is the only way in if the password is lost
sudo fdesetup enable

# Check it: "FileVault is On."
fdesetup status

# Require the password immediately after sleep or screen saver begins
sysadminctl -screenLock immediate -password -

# Check it: the output should end in "screenLock delay is immediate"
sysadminctl -screenLock status
```

## How to add a new host

1. Copy an existing host directory under `modules/hosts/` (`alpha` for NixOS,
   `beta` for macOS) and change `hostName`.
2. Adjust the module list in its `default.nix`.
3. NixOS: set `device` in `disko.nix`, and replace `hardware.nix` with the
   output of `nixos-generate-config` (step 3 above).
4. Add its secrets file ([SECRETS.md](docs/SECRETS.md#adding-a-new-host)).

## References

Other nix configuration that inspired this one:

- [Misterio77/nix-config](https://github.com/Misterio77/nix-config)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [drupol/infra](https://github.com/drupol/infra)
- [BirdeeHub/birdeeSystems](https://github.com/BirdeeHub/birdeeSystems)
- [Goxore/nixconf](https://github.com/Goxore/nixconf)

Also, for more information on the dendritic pattern see:

- [Dendritic Design](https://github.com/Doc-Steve/dendritic-design-with-flake-parts)
- [Dendrix](https://dendrix.oeiuwq.com/index.html)
