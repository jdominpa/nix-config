{
  self,
  ...
}:
let
  hostName = "beta";
in
{
  flake.modules.darwin.beta = {
    imports =
      with self.modules.darwin;
      [
        basic-cli-tools
        bitwarden
        browser
        emacs
        fonts
        jdominpa
        locale
        nix
        powerProfiles
        shell
        ssh
        terminal
      ]
      ++ [
        {
          home-manager.users.jdominpa = {
            imports = with self.modules.homeManager; [
              bitwarden
              direnv
              emacs
              git
              kanata
              latex
            ];
          };
        }
      ];

    networking = {
      inherit hostName;
      computerName = hostName;
    };
    system.defaults.smb.NetBIOSName = hostName;
  };
}
