{
  flake.modules.homeManager.proton =
    { pkgs, ... }:
    {
      services.protonmail-bridge = {
        enable = true;
        extraPackages = [ pkgs.gnome-keyring ];
      };
    };
}
