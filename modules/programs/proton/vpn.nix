{
  flake.modules.nixos.proton-vpn =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.protonvpn-gui ];
    };

  flake.modules.darwin.proton-vpn = {
    homebrew.casks = [ "protonvpn" ];
  };
}
