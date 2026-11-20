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
      security = {
        # Only wheel may even execute the setuid `sudo` binary.
        sudo.execWheelOnly = true;
        # Only wheel may use `su` (shadow's `su` reads only this PAM service).
        pam.services.su.requireWheel = true;
      };
      # No root password: root is reached only through `sudo`.
      users.users.root.hashedPassword = "!";

      # Without this, anyone at the keyboard can append `init=/bin/sh` or
      # `rd.systemd.debug_shell` to the kernel command line from the boot menu.
      boot.loader.systemd-boot.editor = false;
      # Firmware updates, including the Secure Boot revocation list (dbx).
      services.fwupd.enable = true;

      # Disables kexec and hibernation, both of which let root replace or read
      # back the running kernel.
      security = {
        protectKernelImage = true;
        forcePageTableIsolation = true;
      };

      # A crash is still logged in the journal, but the dump is not stored since
      # it would hold what was in memory (such as browser cookies).
      systemd.coredump.settings.Coredump.Storage = "none";

      boot.kernelParams = [
        # Don't merge slab caches of similar size; makes heap exploitation of
        # one cache less likely to reach objects of another.
        "slab_nomerge"
        # Zero memory on allocation and on free, so freed buffers don't keep
        # their contents.
        "init_on_alloc=1"
        "init_on_free=1"
        # Randomise the page allocator free lists and the kernel stack offset
        # on each syscall.
        "page_alloc.shuffle=1"
        # Bounds checks on copies between kernel and user memory.
        "hardened_usercopy=1"
        "randomize_kstack_offset=on"
        # Remove the fixed-address legacy vsyscall page.
        "vsyscall=none"
        # debugfs exposes a lot of kernel internals. Remove this line if a
        # tool you use (powertop, some GPU debugging tools) complains.
        "debugfs=off"
        # Writes to /proc/<pid>/mem obey the ptrace rules (ptrace_scope below)
        # instead of bypassing them.
        "proc_mem.force_override=ptrace"
        # Use the IOMMU to keep PCIe devices from reading or writing arbitrary
        # memory (DMA attacks), with no gap before the kernel takes over from
        # the firmware. Strict mode invalidates stale mappings at once, at a
        # small I/O cost. Does nothing if VT-d is off in the firmware.
        "amd_iommu=force_isolation"
        "intel_iommu=on"
        "iommu=force"
        "iommu.strict=1"
        "efi=disable_early_pci_dma"
        # The block device under a mounted filesystem cannot be written to, a
        # way of corrupting the filesystem under the kernel's feet.
        "bdev_allow_write_mounted=0"
        # Don't let the CPU's random number generator or the bootloader's seed
        # alone count as enough entropy; both are still mixed in.
        "random.trust_cpu=off"
        "random.trust_bootloader=off"
        # Flush the L1 data cache on context switches for processes that ask
        # for it with prctl(PR_SET_SPECULATION_CTRL); free for everything else.
        "l1d_flush=on"
        # No virtual machines inside virtual machines.
        "kvm-intel.nested=0"
        "kvm-amd.nested=0"
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
        # io_uring has been a steady source of kernel privilege escalations and
        # nothing here uses it.
        "kernel.io_uring_disabled" = 2;
        # Setuid programs never dump core.
        "fs.suid_dumpable" = 0;
        # No magic SysRq keys from the keyboard (NixOS allows only sync, 16).
        "kernel.sysrq" = 0;
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

      # Unused kernel code that any unprivileged process can otherwise get
      # auto-loaded with a single `socket()` call, or a USB drive with a single
      # (auto)mount. `install ... false` blocks explicit `modprobe` too, which a
      # plain blacklist entry does not. Names must match the running kernel's
      # modules: a name it does not have blocks nothing.
      boot.extraModprobeConfig = lib.concatMapStrings blockModule [
        # Network protocols.
        "appletalk"
        "atm"
        "ax25"
        "caif_socket"
        "can"
        "hv_sock" # Hyper-V guests
        "ieee802154_socket"
        "kcm"
        "llc2"
        "mpls_router"
        "n_hdlc" # a TTY line discipline
        "netrom"
        "nfc"
        "phonet"
        "pppox"
        "psnap"
        "qrtr"
        "rds"
        "rose"
        "rxrpc"
        "sctp"
        "smc"
        "tipc"
        "vmw_vsock_vmci_transport" # VMware guests
        "x25"
        # IPsec and L2TP. esp4/esp6/rxrpc were the vectors for the Dirty Frag LPE
        # (CVE-2026-43284, CVE-2026-43500). Remove this group if an IPsec VPN is
        # ever needed.
        "af_key"
        "ah4"
        "ah6"
        "esp4"
        "esp6"
        "ip6_vti"
        "ip_vti"
        "ipcomp"
        "ipcomp6"
        "l2tp_core"
        "l2tp_eth"
        "l2tp_ip"
        "l2tp_ip6"
        "l2tp_netlink"
        "l2tp_ppp"
        "xfrm_user"
        # Filesystems nothing here uses, including network ones (gvfs reaches
        # SMB shares from user space). The common disk formats stay, HFS+
        # included.
        "9p"
        "adfs"
        "affs"
        "befs"
        "bfs"
        "ceph"
        "cifs"
        "coda"
        "cramfs"
        "ecryptfs"
        "efs"
        "freevxfs"
        "gfs2"
        "hpfs"
        "jffs2"
        "jfs"
        "kafs"
        "ksmbd"
        "minix"
        "nfs"
        "nfsv3"
        "nfsv4"
        "nilfs2"
        "ocfs2"
        "ocfs2_dlmfs"
        "omfs"
        "orangefs"
        "qnx4"
        "qnx6"
        "romfs"
        "ubifs"
        "ufs"
        "vboxsf"
        "zonefs"
      ];

      # /tmp in RAM, mounted nosuid and nodev, and emptied on every boot.
      boot.tmp.useTmpfs = true;

      networking = {
        firewall.enable = true;
        # Only take effect if NetworkManager is enabled.
        networkmanager = {
          # A new random MAC address on every internet connection. For a wifi
          # network that needs a fixed one: `nmcli connection modify <name>
          # 802-11-wireless.cloned-mac-address permanent`.
          ethernet.macAddress = "random";
          wifi = {
            macAddress = "random";
            scanRandMacAddress = true;
          };
          # IPv6 temporary addresses for outgoing connections.
          connectionConfig."ipv6.ip6-privacy" = 2;
        };
      };

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
