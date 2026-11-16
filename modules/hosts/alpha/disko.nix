{
  inputs,
  ...
}:
let
  system = {
    type = "disk";
    device = "/dev/disk/by-id/nvme-Lexar_SSD_NQ790_2TB_NHC261R003133P220B";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          type = "EF00";
          size = "1G";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [
              "fmask=0177" # file mask: 777-177=600 (owner rw-, group/others ---)
              "dmask=0077" # directory mask: 777-077=700 (owner rwx, group/others ---)
              "noexec" # no execution
              "nosuid" # ignore setuid
              "nodev" # no device nodes
            ];
          };
        };
        luks = {
          size = "100%";
          content = {
            type = "luks";
            name = "crypted";
            settings = {
              # TRIM for SSDs; slightly less secure, better performance
              allowDiscards = true;
              # Try the TPM2 keyslot first; the passphrase still works as
              # recovery
              crypttabExtraOpts = [ "tpm2-device=auto" ];
            };
            # Add boot.initrd.luks.devices so initrd prompts for the PIN at boot
            initrdUnlock = true;
            extraFormatArgs = [
              "--type luks2"
              "--cipher aes-xts-plain64"
              "--hash sha512"
              "--iter-time 5000"
              "--key-size 512"
              "--pbkdf argon2id"
              "--use-random" # Block until enough entropy from /dev/random
            ];
            content = {
              type = "btrfs";
              extraArgs = [ "-f" ]; # Force overwrite if filesystem already exists
              subvolumes = {
                "@" = {
                  mountpoint = "/";
                  mountOptions = [
                    "compress=zstd:1"
                    "noatime"
                  ];
                };
                "@nix" = {
                  mountpoint = "/nix";
                  mountOptions = [
                    "compress=zstd:1"
                    "noatime"
                  ];
                };
                "@home" = {
                  mountpoint = "/home";
                  mountOptions = [
                    "compress=zstd:1"
                    "noatime"
                    "nosuid" # ignore setuid
                    "nodev" # no device nodes
                  ];
                };
                "@snapshots" = {
                  mountpoint = "/.snapshots";
                  mountOptions = [
                    "compress=zstd:1"
                    "noatime"
                    "nosuid" # ignore setuid
                    "nodev" # no device nodes
                    "noexec" # no execution
                  ];
                };
                "@swap" = {
                  mountpoint = "/swap";
                  swap.swapfile.size = "4G";
                };
              };
            };
          };
        };
      };
    };
  };
  data = {
    type = "disk";
    device = "/dev/disk/by-id/nvme-WDC_WD_BLACK_SDBPNTY-1T00-1106_210458800096";
    content = {
      type = "gpt";
      partitions.luks = {
        size = "100%";
        content = {
          type = "luks";
          name = "cryptdata";
          settings = {
            allowDiscards = true;
            # Boot does not wait for it if the disk is missing
            crypttabExtraOpts = [
              "nofail"
              "tpm2-device=auto"
            ];
          };
          # Unlocked in the initrd too: systemd reuses the PIN typed for the
          # system disk
          initrdUnlock = true;
          extraFormatArgs = [
            "--type luks2"
            "--cipher aes-xts-plain64"
            "--hash sha512"
            "--iter-time 5000"
            "--key-size 512"
            "--pbkdf argon2id"
            "--use-random"
          ];
          content = {
            type = "btrfs";
            extraArgs = [ "-f" ];
            subvolumes = {
              "@games" = {
                mountpoint = "/data/games";
                mountOptions = [
                  "compress=zstd:1"
                  "noatime"
                  "nofail"
                  "nosuid" # ignore setuid
                  "nodev" # no device nodes
                ];
              };
              "@backup" = {
                mountpoint = "/data/backup";
                mountOptions = [
                  "compress=zstd:1"
                  "noatime"
                  "nofail"
                  "nosuid" # ignore setuid
                  "nodev" # no device nodes
                  "noexec" # no execution
                ];
              };
            };
          };
        };
      };
    };
  };
in
{
  # The system/data disks so that they can be formatted individually with
  # `disko --mode destroy,format,mount --flake .#alpha-<system/data>`
  flake.diskoConfigurations = {
    alpha-system.disko.devices.disk = { inherit system; };
    alpha-data.disko.devices.disk = { inherit data; };
  };

  flake.modules.nixos.alpha = {
    imports = [ inputs.disko.nixosModules.disko ];
    disko.devices.disk.system = system;
    disko.devices.disk.data = data;
  };
}
