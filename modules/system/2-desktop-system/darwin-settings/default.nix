{
  self,
  ...
}:
{
  flake.modules.darwin.darwin-settings = {
    imports = with self.modules.darwin; [
      dock
      keyboard
      trackpad
    ];
  };
}
