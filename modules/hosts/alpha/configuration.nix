{
  inputs,
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
      ++ (with inputs.self.modules.nixos; [
        jdominpa
      ]);

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

      # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
      system.stateVersion = "24.05";
    };
}
