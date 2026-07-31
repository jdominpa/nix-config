{
  flake.modules.nixos.discord =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.vesktop ];
    };

  flake.modules.darwin.discord =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.vesktop ];
    };
}
