{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-gpu-nvidia
    inputs.hardware.nixosModules.common-pc-ssd
    ./hardware-configuration.nix
    ../common
  ];

  # Bootloader
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  # Nvidia settings needed for nixos-hardware
  hardware = {
    graphics.enable = true;
    nvidia = {
      open = true;
      prime = {
        amdgpuBusId = "PCI:6:0:0";
        nvidiaBusId = "PCI:1:0:0";
      };
    };
  };
  pipewire.enable = true;

  networking = {
    hostName = "alpha";
    networkmanager.enable = true;
  };

  # User
  jdominpa.enable = true;

  # Sddm display manager
  sddm.enable = true;

  # KDE Plasma Desktop Environment.
  plasma.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # System specific packages
  environment.systemPackages = with pkgs; [
    headsetcontrol # Control logitech headset
    piper # Control logitech mice
  ];

  # Needed for piper
  services.ratbagd.enable = true;

  # Steam
  steam.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
