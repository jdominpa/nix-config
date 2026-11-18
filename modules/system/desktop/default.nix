{
  self,
  ...
}:
{
  flake.modules.nixos.desktop = {
    imports = with self.modules.nixos; [
      core
      desktop-tools
      gtk
      login-manager
      niri
      nixpak
      noctalia
      user-dirs
    ];
  };

  flake.modules.darwin.desktop = {
    imports = with self.modules.darwin; [
      core
      darwin-settings
      desktop-tools
    ];
  };
}
