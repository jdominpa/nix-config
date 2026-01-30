{
  self,
  ...
}:
{
  flake.modules.darwin.paneru = {
    imports = with self.modules.darwin; [
      dock
      keyboard
      trackpad
    ];
  };
}
