{
  self,
  ...
}:
{
  flake.modules.nixos.base-system = {
    imports = with self.modules.nixos; [
      bluetooth
      locale
      pipewire
      powerProfiles
      printing
      ssh
      vpn
    ];
  };

  flake.modules.darwin.base-system = {
    imports = with self.modules.darwin; [
      base-settings
      locale
      powerProfiles
      ssh
      vpn
    ];
  };
}
