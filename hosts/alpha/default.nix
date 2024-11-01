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
    ../common/global
    ../common/optional
    ../common/users
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

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.desktopManager.plasma6.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # List packages installed in system profile.
  # To search, run:
  # $ nix search program
  environment.systemPackages = with pkgs; [
    brave
    emacs
    git
    headsetcontrol # Control logitech headset
    stable.iosevka-comfy.comfy
    neovim
    ripgrep
    tree
    unrar
  ];

  # Steam
  steam.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
