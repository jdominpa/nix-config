{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  cfg = config.jdp.home.services.syncthing;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    services.syncthing.enable = lib.mkEnableOption "Whether to enable Syncthing.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      services.syncthing = {
        enable = true;
        tray.enable = isLinux; # tray is only available for Linux
        overrideDevices = true;
        overrideFolders = true;
        settings = {
          devices = {
            alpha = {
              id = "HTFOJLK-MWZSXXP-FVIXWQT-ESTPX5T-IWS2FLK-SHRXJDE-TAOFYAD-5XYJGQS";
            };
            beta = {
              id = "L7XZTU5-7OKWFMG-WCPA6A7-BK6GBGD-AQWIPPH-SM4CCQF-NQ4FS3J-QAEL5AL";
            };
          };
          folders = {
            documents = {
              label = "Documents";
              path = "${user.homeDirectory}/Documents";
              devices = [
                "alpha"
                "beta"
              ];
            };
          };
        };
      };
    };
  };
}
