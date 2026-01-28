{
  self,
  ...
}:
{
  flake.modules.nixos.niri-desktop = {
    imports = with self.modules.nixos; [
      desktop-tools
      login-manager
      niri
      noctalia
    ];
  };

  flake.modules.homeManager.niri-desktop = {
    imports = with self.modules.homeManager; [
      niri
      noctalia
    ];
  };
}
