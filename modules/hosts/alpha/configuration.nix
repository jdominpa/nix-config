{
  inputs,
  self,
  ...
}:
{
  flake.modules.nixos.alpha =
    { pkgs, ... }:
    {
      imports = [
        inputs.nixos-hardware.nixosModules.common-cpu-intel
        inputs.nixos-hardware.nixosModules.common-pc-ssd
      ]
      ++ (with self.modules.nixos; [
        bluetooth
        cli-tools
        emacs
        fonts
        jdominpa
        locale
        niri-desktop
        nix
        pipewire
        powerProfiles
        printing
        shell
        ssh
        steam
      ])
      ++ [
        {
          home-manager.users.jdominpa = {
            imports = with self.modules.homeManager; [
              emacs
              niri-desktop
              shell
            ];
          };
        }
      ];

      boot.loader = {
        efi.canTouchEfiVariables = true;
        # TODO: check systemd-boot.windows option
        systemd-boot = {
          enable = true;
          # Don't keep to many generations
          configurationLimit = 10;
        };
      };

      environment.systemPackages = with pkgs; [
        headsetcontrol # Control logitech headset
        piper # Control logitech mice
      ];

      # Nvidia drivers settings
      hardware = {
        graphics.enable = true;
        nvidia = {
          open = false;
          modesetting.enable = true;
        };
      };

      # Niri output monitors
      home-manager.users.jdominpa = {
        programs.niri.settings = {
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
      };

      networking = {
        hostName = "alpha";
        networkmanager.enable = true;
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

      # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
      system.stateVersion = "24.05";
    };
}
