{
  flake.modules.homeManager.jdominpa =
    { config, pkgs, ... }:
    let
      mailDir = "${config.home.homeDirectory}/Mail";
      inherit (pkgs.stdenv.hostPlatform) isLinux;
    in
    {
      accounts.email = {
        accounts = {
          mail = {
            enable = true;
            primary = true;
            address = "work@jdompas.com";
            userName = "work@jdompas.com";
            realName = "Joan Domingo Pasarin";
            passwordCommand = "secret-tool lookup service imap account work@jdompas.com";
            imap = {
              host = "127.0.0.1";
              port = 1143;
              tls = {
                enable = true;
                useStartTls = true;
                certificatesFile = "${mailDir}/certificates/cert.pem";
              };
            };
            smtp = {
              host = "127.0.0.1";
              port = 1025;
              tls = {
                enable = true;
                useStartTls = true;
                certificatesFile = "${mailDir}/certificates/cert.pem";
              };
            };
            mbsync = {
              enable = true;
              create = "both";
              expunge = "both";
              patterns = [
                "*"
                "!All Mail"
              ];
              subFolders = "Verbatim";
              extraConfig = {
                account = {
                  AuthMechs = "LOGIN";
                  CertificateFile = "${mailDir}/certificates/cert.pem";
                };
                channel = {
                  Sync = "All";
                  SyncState = "*";
                  CopyArrivalDate = "yes";
                };
              };
            };
            msmtp = {
              enable = true;
              extraConfig = {
                auth = "on";
                tls = "on";
                tls_trust_file = "${mailDir}/certificates/cert.pem";
                logfile = "${mailDir}/msmtp.log";
              };
            };
            mu.enable = true;
          };
          git = {
            address = "work@jdompas.com";
            realName = "Joan Domingo Pasarin";
          };
        };
        maildirBasePath = mailDir;
      };
      programs = {
        mbsync.enable = true;
        msmtp.enable = true;
        mu.enable = true;
      };
      services.mbsync.enable = isLinux;
    };
}
