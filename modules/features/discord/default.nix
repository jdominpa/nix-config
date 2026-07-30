{
  flake.modules.nixos.discord =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.vencord ];
    };

  flake.modules.darwin.discord =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.vencord ];
    };
}
