[Dendritic](https://github.com/mightyiam/dendritic) NixOS/macOS configuration
using [nix](https://nixos.org), [flakes](https://nixos.wiki/wiki/Flakes),
[nix-darwin](https://github.com/nix-darwin/nix-darwin) and
[home-manager](https://nixos.wiki/wiki/Home_Manager).

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

Also, for more information on the dendritic pattern see:

- [Dendritic Design](https://github.com/Doc-Steve/dendritic-design-with-flake-parts)
- [Dendrix](https://dendrix.oeiuwq.com/index.html)
