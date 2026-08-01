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
      noctalia-shell
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
