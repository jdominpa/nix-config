{
  flake.modules.homeManager.cli-mail =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [
        isync
        msmtp
      ];
      programs.notmuch = {
        enable = true;
        hooks = {
          preNew = "mbsync -a";
        };
        new = {
          tags = [
            "unread"
            "inbox"
          ];
          ignore = [
            ".mbsyncstate"
            ".uidvalidity"
          ];
        };
        extraConfig = {
          database = {
            path = "${config.xdg.dataHome}/mail";
          };
          user = {
            name = "Joan Domingo Pasarin";
            primary_email = "jdompas@proton.me";
          };
        };
      };
      services.mbsync.enable = true;
    };
}
