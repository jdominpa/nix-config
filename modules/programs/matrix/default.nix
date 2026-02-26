{
  flake.modules.nixos.matrix =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.element-desktop ];
    };
}
