{
  self,
  ...
}:
{
  flake.nixosModules.desktop = {
    imports = with self.nixosModules; [
      core
      desktop-tools
      login-manager
      noctalia
    ];
  };

  flake.darwinModules.desktop = {
    imports = with self.darwinModules; [
      core
      darwin-settings
      desktop-tools
    ];
  };
}
