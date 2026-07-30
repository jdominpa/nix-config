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
      noctalia
    ] ++ (with self.nixosModules; [
      niri
    ]);
  };

  flake.modules.darwin.desktop-system = {
    imports = with self.modules.darwin; [
      base-system
      darwin-settings
      desktop-tools
    ];
  };
}
