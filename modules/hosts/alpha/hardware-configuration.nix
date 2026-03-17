{
  inputs,
  ...
}:
{
  flake.modules.nixos.alpha =
    {
      config,
      lib,
      modulesPath,
      ...
    }:
    {
      imports = [
        (modulesPath + "/installer/scan/not-detected.nix")
        inputs.disko.nixosModules.disko
      ];

      boot.initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "nvme"
        "usb_storage"
        "usbhid"
        "sd_mod"
      ];
      boot.initrd.kernelModules = [ ];
      boot.kernelModules = [ "kvm-intel" ];
      boot.extraModulePackages = [ ];

      disko.devices.disk.main = {
        type = "disk";
        device = "/dev/nvme0n1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
              size = "512M";
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
                  "--key-size 256"
                  "--pbkdf argon2id"
                  "--use-random" # Block until enough entropy from /dev/random
                ];
                content = {
                  type = "btrfs";
                  extraArgs = [ "-f" ]; # Force overwrite if filesystem already exists
                  subvolumes = {
                    # Top-level subvolume (id 5); used for btrfs send/receive and snapshots
                    "/" = {
                      mountpoint = "/btr_pool";
                      mountOptions = [ "subvolid=5" ];
                    };
                    "@" = {
                      mountpoint = "/";
                      mountOptions = [ "compress=zstd:1" ];
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
                      mountOptions = [ "compress=zstd:1" ];
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

      # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
      # (the default) this is the recommended approach. When using systemd-networkd it's
      # still possible to use this option, but it's recommended to use it in conjunction
      # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
      networking.useDHCP = lib.mkDefault true;
      # networking.interfaces.enp3s0.useDHCP = lib.mkDefault true;
      # networking.interfaces.wlo1.useDHCP = lib.mkDefault true;

      nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
      hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    };
}
