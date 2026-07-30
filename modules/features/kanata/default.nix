{
  flake.darwinModules.kanata =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.kanata ];
      homebrew.casks = [ "karabiner-elements" ];
    };
}
