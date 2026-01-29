{
  self,
  ...
}:
{
  flake.modules.nixos.scrolling-desktop = {
    imports = with self.modules.nixos; [
      desktop-tools
      login-manager
      niri
      noctalia
    ];
  };
}
