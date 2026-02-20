{
  flake.modules.nixos.mullvad-vpn =
    { pkgs, ... }:
    {
      services.mullvad-vpn = {
        enable = true;
        package = pkgs.mullvad-vpn;
      };
    };

  flake.modules.darwin.mullvad-vpn =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.mullvad-vpn ];
    };
}
