{
  flake.modules.nixos.btrbk = {
    services.btrbk.instances.btrbk = {
      # How often this btrbk instance is started. See systemd.time(7) for more information about the format.
      onCalendar = "daily";
      settings = {
        # how to prune local snapshots:
        # 1. keep daily snapshots for 7 days, weekly for 4 weeks
        snapshot_preserve = "7d 4w";
        # 2. keep all snapshots for 2 days, no matter how frequently you (or your cron job) run btrbk
        snapshot_preserve_min = "2d";
        volume."/btr_pool" = {
          snapshot_dir = "snapshots";
          subvolume = {
            "@home" = {};
          };
        };
      };
    };
  };
}
