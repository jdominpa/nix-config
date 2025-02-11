{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
with lib;
let
  hostName = "alpha";
  user = config.jdp.base.user;
in
{
  imports = [
    ./hardware-configuration.nix
    inputs.hardware.nixosModules.common-cpu-intel
    inputs.hardware.nixosModules.common-gpu-nvidia
    inputs.hardware.nixosModules.common-pc-ssd
    (jdp.relativeToRoot "modules/base")
    (jdp.relativeToRoot "modules/nixos")
    (jdp.relativeToRoot "modules/home")
  ];

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

  jdp = {
    base = {
      system = {
        nix.enable = true;
        packages.enable = true;
        shell.aliases.enable = true;
      };
      user = {
        enable = true;
        name = "jdominpa";
        fullName = "Joan Domingo Pasarin";
        email = "jdomingopasarin@icloud.com";
        shell = pkgs.zsh;
        homeDirectory = "/home/${user.name}";
        home-manager.enable = true;
      };
    };
    nixos = {
      apps = {
        steam.enable = true;
      };
      desktop = {
        plasma.enable = true;
      };
      services = {
        bluetooth.enable = true;
        openssh.enable = true;
        power.enable = true;
        sddm.enable = true;
      };
      system = {
        boot = {
          systemd.enable = true;
        };
        fonts.enable = true;
        locale = {
          enable = true;
          timezone = "Europe/Madrid";
        };
        networking = {
          inherit hostName;
          networkmanager.enable = true;
        };
        nix.enable = true;
        pipewire.enable = true;
        shell.zsh.enable = true;
        utils.enable = true;
      };
    };
    home = {
      apps = {
        bitwarden.enable = true;
        brave.enable = true;
        discord.enable = true;
      };
      cli = {
        direnv.enable = true;
        fzf.enable = true;
        git.enable = true;
        starship.enable = true;
        zsh.enable = true;
      };
      desktop.plasma-manager = {
        enable = true;
        input.mice = [
          {
            name = "Logitech G502 HERO Gaming Mouse";
            enable = true;
            accelerationProfile = "none";
            productId = "c08b";
            vendorId = "046d";
          }
        ];
      };
      editors.emacs = {
        enable = true;
        withLsp = true;
      };
    };
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Peripherials
  environment.systemPackages = with pkgs; [
    headsetcontrol # Control logitech headset
    piper # Control logitech mice
  ];

  # Needed for piper
  services.ratbagd.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
