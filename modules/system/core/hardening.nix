{
  config,
  ...
}:
let
  inherit (config.user) username;
in
{
  flake.modules.nixos.hardening =
    { lib, pkgs, ... }:
    let
      blockModule = module: "install ${module} ${lib.getExe' pkgs.coreutils "false"}\n";
    in
    {
      # Only wheel may even execute the setuid `sudo` binary.
      security.sudo.execWheelOnly = true;

      # Without this, anyone at the keyboard can append `init=/bin/sh` or
      # `rd.systemd.debug_shell` to the kernel command line from the boot menu.
      boot.loader.systemd-boot.editor = false;
      # Firmware updates, including the Secure Boot revocation list (dbx).
      services.fwupd.enable = true;

      # Disables kexec and hibernation, both of which let root replace or read
      # back the running kernel.
      security.protectKernelImage = true;
      security.forcePageTableIsolation = true;

      boot.kernelParams = [
        # Don't merge slab caches of similar size; makes heap exploitation of
        # one cache less likely to reach objects of another.
        "slab_nomerge"
        # Zero memory on allocation. `init_on_free=1` is the stronger sibling
        # but costs a few percent in allocation-heavy workloads. Worth it if it
        # doesn't affect performance significantly.
        "init_on_alloc=1"
        # Randomise the page allocator free lists and the kernel stack offset
        # on each syscall.
        "page_alloc.shuffle=1"
        "randomize_kstack_offset=on"
        # Remove the fixed-address legacy vsyscall page.
        "vsyscall=none"
        # debugfs exposes a lot of kernel internals. Remove this line if a
        # tool you use (powertop, some GPU debugging tools) complains.
        "debugfs=off"
      ];

      boot.kernel.sysctl = {
        # Hide kernel pointers and the kernel log from unprivileged users.
        "kernel.kptr_restrict" = 2;
        "kernel.dmesg_restrict" = 1;
        # eBPF: root only, and harden the JIT for everyone.
        "kernel.unprivileged_bpf_disabled" = 1;
        "net.core.bpf_jit_harden" = 2;
        # A process may only ptrace its own descendants. `gdb -p` and
        # `strace -p` on an unrelated process need sudo; debugging a program
        # you launch from the debugger is unaffected.
        "kernel.yama.ptrace_scope" = 1;
        # Unprivileged users may profile their own programs, not the kernel.
        "kernel.perf_event_paranoid" = 2;
        # Unprivileged userfaultfd only for faults in user mode; stalling
        # kernel-mode faults is a common aid in exploits.
        "vm.unprivileged_userfaultfd" = 0;
        # Don't auto-load TTY line disciplines and forbid TIOCSTI keystroke
        # injection, both recurring sources of local privilege escalation.
        "dev.tty.ldisc_autoload" = 0;
        "dev.tty.legacy_tiocsti" = 0;
        # Maximum ASLR entropy for mmap.
        "vm.mmap_rnd_bits" = 32;
        "vm.mmap_rnd_compat_bits" = 16;
        # Stricter rules for files in world-writable sticky directories.
        "fs.protected_fifos" = 2;
        "fs.protected_regular" = 2;
        "fs.protected_symlinks" = 1;
        "fs.protected_hardlinks" = 1;

        # Network: this machine is not a router.
        "net.ipv4.conf.all.accept_redirects" = 0;
        "net.ipv4.conf.default.accept_redirects" = 0;
        "net.ipv4.conf.all.secure_redirects" = 0;
        "net.ipv4.conf.default.secure_redirects" = 0;
        "net.ipv4.conf.all.send_redirects" = 0;
        "net.ipv4.conf.default.send_redirects" = 0;
        "net.ipv6.conf.all.accept_redirects" = 0;
        "net.ipv6.conf.default.accept_redirects" = 0;
        "net.ipv4.conf.all.accept_source_route" = 0;
        "net.ipv4.conf.default.accept_source_route" = 0;
        "net.ipv6.conf.all.accept_source_route" = 0;
        "net.ipv6.conf.default.accept_source_route" = 0;
        "net.ipv4.tcp_syncookies" = 1;
        "net.ipv4.tcp_rfc1337" = 1;
        "net.ipv4.icmp_echo_ignore_broadcasts" = 1;
      };

      # Obscure network protocols and filesystems (plus `vivid`, a video test
      # driver) that any unprivileged process can otherwise get auto-loaded with
      # a single `socket()` or `mount` call. `install ... false` blocks explicit
      # `modprobe` too, which a plain blacklist entry does not.
      boot.extraModprobeConfig = lib.concatMapStrings blockModule [
        "af_802154"
        "appletalk"
        "atm"
        "ax25"
        "can"
        "dccp"
        "decnet"
        "econet"
        # esp4/esp5/rxrpc were the vectors for the Dirty Frag LPE
        # (CVE-2026-43284, CVE-2026-43500). Remove esp4 and esp6 if an IPsec VPN
        # is ever needed.
        "esp4"
        "esp6"
        "ipx"
        "n-hdlc"
        "netrom"
        "p8022"
        "p8023"
        "psnap"
        "rds"
        "rose"
        "rxrpc"
        "sctp"
        "tipc"
        "x25"
        "cramfs"
        "freevxfs"
        "jffs2"
        "vivid"
      ];

      # /tmp in RAM, mounted nosuid and nodev, and emptied on every boot.
      boot.tmp.useTmpfs = true;

      networking.firewall.enable = true;

      # Only takes effect if sshd is enabled. Key-only logins for one user.
      services.openssh.settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        PermitRootLogin = "no";
        AllowUsers = [ username ];
      };
    };

  flake.modules.darwin.hardening = {
    # The built-in application firewall is off by default on macOS
    networking.applicationFirewall = {
      enable = true;
      enableStealthMode = true;
      # Apple's own services may listen without a prompt...
      allowSigned = true;
      # ...everything else asks once per binary
      allowSignedApp = false;
    };

    system.defaults = {
      loginwindow.GuestEnabled = false;
      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;
    };

    # Only takes effect while Remote Login is on.
    services.openssh.extraConfig = ''
      PasswordAuthentication no
      KbdInteractiveAuthentication no
      PermitRootLogin no
      AllowUsers ${username}
    '';
  };
}
