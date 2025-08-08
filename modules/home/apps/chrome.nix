{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.home.apps.google-chrome;
  inherit (config.jdp.base) user;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in
{
  options.jdp.home = {
    apps.google-chrome.enable = lib.mkEnableOption "Install Google Chrome web browser.";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = isLinux;
        message = "Use homebrew to install Google Chrome on macOS.";
      }
    ];

    home-manager.users.${user.name} = {
      home.packages = [ pkgs.google-chrome ];
    };
  };
}
