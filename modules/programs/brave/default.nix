{
  flake.modules.nixos.browser =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.brave ];
    };

  flake.modules.darwin.browser =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.brave ];
    };
}
