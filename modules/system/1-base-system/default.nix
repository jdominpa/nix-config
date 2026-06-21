{
  self,
  ...
}:
{
  flake.modules.nixos.base-system = {
    imports = with self.modules.nixos; [
      bluetooth
      fonts
      locale
      pipewire
      powerProfiles
      printing
      ssh
    ];
  };

  flake.modules.darwin.base-system = {
    imports = with self.modules.darwin; [
      base-settings
      fonts
      locale
      powerProfiles
      ssh
    ];
  };
}
