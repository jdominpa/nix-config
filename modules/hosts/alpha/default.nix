{
  inputs,
  self,
  ...
}:
let
  hostName = "alpha";
in
{
  flake.nixosConfigurations.${hostName} = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [ self.modules.nixos.${hostName} ];
  };

  flake.modules.nixos.${hostName} =
    { pkgs, ... }:
    {
      imports = [
        inputs.nixos-hardware.nixosModules.common-cpu-intel
        inputs.nixos-hardware.nixosModules.common-pc-ssd
      ]
      ++ (with self.modules.nixos; [
        bitwarden
        brave
        btrbk
        cli-tools
        desktop
        discord
        emacs
        gaming
        git
        home-manager
        jdominpa
        kitty
        latex
        syncthing
        zsh
      ]);

      boot.loader = {
        efi.canTouchEfiVariables = true;
        systemd-boot = {
          enable = true;
          # Don't keep too many generations
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
          open = true;
          modesetting.enable = true;
        };
      };

      networking = {
        inherit hostName;
        networkmanager = {
          enable = true;
          wifi.powersave = false;
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

      # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
      system.stateVersion = "24.05";
    };
}
