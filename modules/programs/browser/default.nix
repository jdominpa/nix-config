{
  flake.modules.nixos.browser =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.chromium ];
    };

  flake.modules.darwin.browser = {
    homebrew.casks = [ "google-chrome" ];
  };
}
