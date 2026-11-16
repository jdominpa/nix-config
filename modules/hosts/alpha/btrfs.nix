{
  # Tied to the subvolumes in `disko.nix`.
  flake.modules.nixos.alpha =
    { lib, ... }:
    {
      # Reads every block and checks it against its checksum
      services.btrfs.autoScrub = {
        enable = true;
        interval = "monthly";
        # One mount per file system, all subvolumes share it
        fileSystems = [
          "/"
          "/data/backup"
        ];
      };
      # Read-only snapshots of `@home` in `/.snapshots`, also sent to the data
      # disk so a copy survives the system disk.
      services.btrbk.instances.home = {
        onCalendar = "hourly";
        settings = {
          backend = "btrfs-progs";
          timestamp_format = "long";
          snapshot_preserve_min = "6h";
          snapshot_preserve = "48h 7d 4w";
          target_preserve_min = "no";
          target_preserve = "14d 8w 6m";
          subvolume."/home" = {
            snapshot_dir = "/.snapshots";
            target = "/data/backup";
          };
        };
      };
      systemd.services.btrbk-home = {
        # Without the data disk, `/data/backup` is a plain directory on the
        # system disk and btrbk would fill it. Fail the run instead.
        unitConfig.RequiresMountsFor = [ "/data/backup" ];
        # The module runs btrbk as the `btrbk` user through sudo, which
        # `security.sudo.execWheelOnly` blocks. Run it as root instead.
        serviceConfig = {
          User = lib.mkForce "root";
          Group = lib.mkForce "root";
        };
      };
    };
}
