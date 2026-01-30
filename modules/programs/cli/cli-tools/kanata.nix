{
  flake.modules.homeManager.kanata =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.kanata ];
    };
}
