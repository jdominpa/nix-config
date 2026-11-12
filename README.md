[Dendritic](https://github.com/mightyiam/dendritic) NixOS/macOS configuration
using [nix](https://nixos.org), [flakes](https://nixos.wiki/wiki/Flakes),
[nix-darwin](https://github.com/nix-darwin/nix-darwin) and
[nix-wrapper-modules](https://github.com/BirdeeHub/nix-wrapper-modules).

## Deployment

### NixOS

This steps assume that the NixOS configuration for the machine already exists in
the flake. Otherwise you will have to add it after cloning the flake in Step 2.

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

Identify the name of the system disk using `lsblk` and make sure the `disko`
configuration for the host you want to install is set to that disk. Then, format
the disk with the following command:

```bash
sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko/latest -- \
  --mode destroy,format,mount \
  --flake /tmp/nix-config#<hostname>
```

#### 3. Generate hardware configuration

Generate the hardware configuration for the machine with the command
`nixos-generate-config`. The flag `--no-filesystems` is used to skip the
filesystem configuration since that is managed with `disko`.

```bash
sudo nixos-generate-config --no-filesystems --root /mnt
```

After that, edit the host's hardware configuration `hardware.nix` of the flake
and update it with the hardware configuration generated.

#### 4. Install NixOS

A host that imports the `secure-boot` module signs its boot files during the
installation, so its signing keys have to be in place before `nixos-install`
runs. Skip this for any other host.

```bash
# Create the Secure Boot keys and copy them to the new system
sudo nix --experimental-features "nix-command flakes" run nixpkgs#sbctl -- create-keys
sudo mkdir -p /mnt/var/lib
sudo cp -a /var/lib/sbctl /mnt/var/lib/
```

```bash
# Install NixOS with <hostname> configuration
sudo nixos-install --flake /tmp/nix-config#<hostname> --root /mnt

# Set user password for any users defined in the configuration
sudo nixos-enter --root /mnt -c 'passwd <username>'

# Copy the flake (with hardware-config.nix changes) to the machine's filesystem
sudo cp -r /tmp/nix-config /mnt/etc/nixos/nix-config

# Unmount and reboot
sudo umount -R /mnt
reboot
```

#### 5. Enable Secure Boot

Only for hosts that import the `secure-boot` module, which replaces systemd-boot
with [lanzaboote](https://github.com/nix-community/lanzaboote). After Step 4 the
boot files are signed, but the firmware does not know the keys yet.

To add Secure Boot to a host that was installed without the module, create the
keys with `sudo nix run nixpkgs#sbctl -- create-keys`, add `secure-boot` to the
imports of the host and rebuild. Then continue here.

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
   Microsoft next to them: the firmware of the graphics card is usually signed
   with those, and without them the machine may not boot anymore.

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

The easiest way to add a new host is to copy and adapt an existing configuration:

1. Create a new folder under `modules/hosts/` with the name of the new host.
2. Copy `flake-parts.nix` from another NixOS/macOS host to the new folder. Make
sure you also change the hostname in the file to the new hostname.
3. For NixOS, copy `/etc/nixos/configuration.nix` and
`/etc/nixos/hardware-configuration.nix` to the new folder and edit
`configuration.nix` to import any needed modules. For macOS, copy
`configuration.nix` from another macOS configuration and add/remove any needed
modules.
4. Edit `configuration.nix` and `hardware-configuration.nix` to make sure that
both files are `flake-parts` modules

**NOTE**: for NixOS, the host's disko configuration has to be added to
 `hardware-configuration.nix` as well.

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
