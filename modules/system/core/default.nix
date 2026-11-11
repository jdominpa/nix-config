{
  self,
  ...
}:
{
  flake.modules.nixos.core = {
    imports = with self.modules.nixos; [
      bluetooth
      fonts
      hardening
      locale
      nix
      pipewire
      power-profiles
      printing
      ssh
      user
    ];
  };

  flake.modules.darwin.core = {
    imports = with self.modules.darwin; [
      fonts
      hardening
      homebrew
      locale
      nix
      power-profiles
      ssh
      user
    ];
  };
}
