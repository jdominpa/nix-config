{
  self,
  ...
}:
{
  flake.modules.nixos.core = {
    imports = with self.modules.nixos; [
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

  flake.modules.darwin.core = {
    imports = with self.modules.darwin; [
      fonts
      homebrew
      locale
      nix
      power-profiles
      ssh
    ];
  };
}
