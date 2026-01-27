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
        cli-tools
        emacs
        jdominpa
        nix
        shell
      ])
      ++ [
        {
          home-manager.users.jdominpa = {
            imports = with self.modules.homeManager; [
              emacs
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
