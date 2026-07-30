{
  self,
  ...
}:
{
  flake.nixosModules.core = {
    imports = with self.nixosModules; [
      bluetooth
      fonts
      locale
      pipewire
      power-profiles
      printing
      ssh
    ];
  };

  flake.darwinModules.core = {
    imports = with self.darwinModules; [
      base-settings
      fonts
      locale
      power-profiles
      ssh
    ];
  };
}
