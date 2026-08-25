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
        kitty
        latex
        syncthing
        zsh
      ]);

      # Niri monitor setup
      wrappers.niri.settings.outputs = {
        "DP-1" = {
          mode = "1920x1080@143.855";
          position = _: {
            props = {
              x = 2560;
              y = 180;
            };
          };
          scale = 1.0;
          variable-refresh-rate = _: { };
        };
        "DP-2" = {
          focus-at-startup = _: { };
          mode = "2560x1440@240.001";
          position = _: {
            props = {
              x = 0;
              y = 0;
            };
          };
          scale = 1.0;
          variable-refresh-rate = _: { };
        };
      };

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
