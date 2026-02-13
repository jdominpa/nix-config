{
  self,
  ...
}:
{
  flake.modules.nixos.desktop-system = {
    imports = with self.modules.nixos; [
      base-system
      desktop-tools
      login-manager
      niri
      noctalia
    ];
  };

  flake.modules.darwin.desktop-system = {
    imports = with self.modules.darwin; [
      base-system
      paneru
    ];
  };
}
