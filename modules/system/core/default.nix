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
      nix
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
      homebrew
      locale
      nix
      power-profiles
      ssh
    ];
  };
}
