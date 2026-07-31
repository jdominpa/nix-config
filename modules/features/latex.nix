{
  flake.modules.nixos.latex =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.texliveFull ];
    };

  flake.modules.darwin.latex =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.texliveFull ];
    };
}
