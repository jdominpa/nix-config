{
  self,
  ...
}:
{
  flake.modules.nixos.browser = {
    imports = [ self.modules.nixos.zen-browser ];
  };

  flake.modules.darwin.browser = {
    imports = [ self.modules.darwin.zen-browser ];
  };
}
