{
  flake.modules.nixos.browser =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.google-chrome ];
    };

  flake.modules.darwin.browser =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.google-chrome ];
    };
}
