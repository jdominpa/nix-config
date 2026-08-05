{
  flake.modules.darwin.kanata =
    { lib, pkgs, ... }:
    let
      # nixpkgs pins the exact driver version kanata was built against
      driver = pkgs.kanata.darwinDriver;
      managerDir = "/Applications/.Nix-Kanata";
      manager = "${managerDir}/.Karabiner-VirtualHIDDevice-Manager.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Manager";
      daemon = "${driver}/Library/Application Support/org.pqrs/Karabiner-DriverKit-VirtualHIDDevice/Applications/Karabiner-VirtualHIDDevice-Daemon.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Daemon";
    in
    {
      environment.systemPackages = [ pkgs.kanata ];

      system.activationScripts.preActivation.text = ''
        rm -rf ${managerDir}
        mkdir -p ${managerDir}
        # Kernel extensions reside in /Applications, they cannot be symlinks
        cp -r "${driver}/Applications/.Karabiner-VirtualHIDDevice-Manager.app" ${managerDir}
      '';

      system.activationScripts.postActivation.text = ''
        echo "activating the Karabiner virtual HID driver extension" >&2
        launchctl kickstart -k system/org.nixos.karabiner-vhid-activate || true
      '';

      # forceActivate ensures that the extension is activated even if its
      # version is older than an already existing one.
      launchd.daemons.karabiner-vhid-activate = {
        serviceConfig = {
          Label = "org.nixos.karabiner-vhid-activate";
          ProgramArguments = [
            manager
            "forceActivate"
          ];
          RunAtLoad = true;
          # Log reports a refused replacement or a pending user approval.
          StandardOutPath = "/var/log/karabiner-vhid-activate.log";
          StandardErrorPath = "/var/log/karabiner-vhid-activate.log";
        };
      };

      # The daemon reads no input, so it needs no privacy permission of its own
      # and can be started indirectly.
      launchd.daemons.karabiner-vhid-daemon = {
        serviceConfig = {
          Label = "org.pqrs.Karabiner-VirtualHIDDevice-Daemon";
          ProgramArguments = [
            "/bin/sh"
            "-c"
            # Wait for the nix store to be mounted before executing the daemon
            "/bin/wait4path /nix/store && exec ${lib.escapeShellArg daemon}"
          ];
          RunAtLoad = true;
          KeepAlive = true;
          StandardOutPath = "/var/log/karabiner-vhid-daemon.log";
          StandardErrorPath = "/var/log/karabiner-vhid-daemon.log";
        };
      };
    };
}
