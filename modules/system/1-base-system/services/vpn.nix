{
  flake.modules.nixos.vpn =
    { pkgs, ... }:
    {
      services.mullvad-vpn = {
        enable = true;
        enableEarlyBootBlocking = true;
        package = pkgs.mullvad-vpn;
      };
    };

  flake.modules.darwin.vpn = {
    homebrew.casks = [ "mullvad-vpn" ];
  };
}
