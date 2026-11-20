{
  # Time from NTS servers instead of systemd-timesyncd's unauthenticated
  # pool.ntp.org, so a device on the network cannot shift the clock (which
  # would make expired or revoked TLS certificates look valid). Enabling chrony
  # turns timesyncd off.
  flake.modules.nixos.chrony =
    { config, pkgs, ... }:
    {
      services.chrony = {
        enable = true;
        enableNTS = true;
        # Six operators, so that no single one can supply a majority.
        servers = [
          "time.cloudflare.com"
          "ntppool1.time.nl"
          "nts.netnod.se"
          "ptbtime1.ptb.de"
          "time.dfm.dk"
          "time.cifelli.xyz"
        ];
        makestep.threshold = 1.0;
        # chrony's own seccomp filter, and keep the measurement history across
        # restarts (needs `dumpdir`).
        extraFlags = [
          "-F"
          "1"
          "-r"
        ];
        extraConfig = ''
          # Adjust the clock only when three sources agree, and never use one
          # that is not authenticated.
          minsources 3
          authselectmode require

          dumpdir ${config.services.chrony.directory}
          leapseclist ${pkgs.tzdata}/share/zoneinfo/leap-seconds.list

          # No UDP command port on loopback; `chronyc` works as root through
          # chronyd's Unix socket.
          cmdport 0
        '';
      };
    };
}
