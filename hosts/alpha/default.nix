{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../common
  ];

  # Bootloader
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  nvidia.enable = true;
  pipewire.enable = true;

  networking = {
    hostName = "alpha";
    networkmanager.enable = true;
  };

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
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    brave
    emacs
    git
    neovim
    headsetcontrol # Control logitech headset
    ripgrep
    tree
    unrar
  ];

  # Steam
  steam.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
