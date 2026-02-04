{
  self,
  ...
}:
{
  flake.modules.nixos.browser =
    { pkgs, ... }:
    {
      imports = [ self.modules.nixos.zen-browser ];
      environment.systemPackages = [ pkgs.google-chrome ];
    };

  flake.modules.darwin.browser =
    { pkgs, ... }:
    {
      imports = [ self.modules.darwin.zen-browser ];
      environment.systemPackages = [ pkgs.google-chrome ];
    };
}
