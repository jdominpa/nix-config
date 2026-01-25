{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  hostName = "nixos";
  inherit (config.jdp.base) user;
in
{
  imports = [
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.common-pc-ssd
    (lib.jdp.relativeToRoot "modules/base")
    (lib.jdp.relativeToRoot "modules/nixos")
    (lib.jdp.relativeToRoot "modules/home")
  ];

  # Nvidia drivers settings
  hardware = {
    graphics.enable = true;
    nvidia = {
      open = false;
      modesetting.enable = true;
    };
  };
  services = {
    xserver = {
      videoDrivers = [ "nvidia" ];
      # Configure keymap in X11
      xkb = {
        layout = "us,us";
        options = "grp:shift_caps_toggle";
        variant = ",intl";
      };
    };
    # Needed for piper
    ratbagd.enable = true;
  };

  # Peripherials
  environment.systemPackages = with pkgs; [
    headsetcontrol # Control logitech headset
    piper # Control logitech mice
  ];

  jdp = {
    base = {
      apps = {
        kitty.enable = true;
      };
      editors.emacs.enable = true;
      system = {
        nix.enable = true;
        packages.enable = true;
        shell.aliases.enable = true;
      };
      user = {
        enable = true;
        name = "jdominpa";
        fullName = "Joan Domingo Pasarin";
        email = "jdomingopasarin@gmail.com";
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
        gdm.enable = true;
        niri.enable = true;
        noctalia.enable = true;
      };
      services = {
        bluetooth.enable = true;
        openssh.enable = true;
        power.enable = true;
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
        bitwarden = {
          enable = true;
          sshAgent = true;
        };
        discord.enable = true;
        kitty.enable = true;
        google-chrome.enable = true;
      };
      cli = {
        direnv.enable = true;
        fzf.enable = true;
        git.enable = true;
        starship.enable = true;
        zsh.enable = true;
      };
      desktop.niri = {
        enable = true;
        noctalia.enable = true;
        outputs = {
          "DP-1" = {
            enable = true;
            mode.width = 1920;
            mode.height = 1080;
            mode.refresh = 143.855;
            position.x = 0;
            position.y = 0;
            scale = 1.0;
            variable-refresh-rate = true;
          };
          "DP-2" = {
            enable = true;
            focus-at-startup = true;
            mode.width = 2560;
            mode.height = 1440;
            mode.refresh = 240.001;
            position.x = 1920;
            position.y = 0;
            scale = 1.0;
            variable-refresh-rate = true;
          };
        };
      };
      dev = {
        tex.enable = true;
      };
      editors.emacs = {
        enable = true;
        withLsp = true;
      };
    };
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.05";
}
