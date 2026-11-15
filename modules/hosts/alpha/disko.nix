{
  inputs,
  ...
}:
{
  flake.modules.nixos.alpha = {
    imports = [ inputs.disko.nixosModules.disko ];
    disko.devices.disk.system = {
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
              # TRIM for SSDs; slightly less secure, better performance
              settings.allowDiscards = true;
              # Add boot.initrd.luks.devices so initrd prompts for passphrase at boot
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
  };
}
