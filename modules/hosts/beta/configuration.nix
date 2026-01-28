{
  self,
  ...
}:
{
  flake.modules.darwin.beta = {
    imports =
      with self.modules.darwin;
      [
        cli-tools
        emacs
        fonts
        jdominpa
        locale
        nix
        powerProfiles
        shell
        ssh
      ]
      ++ [
        {
          home-manager.users.jdominpa = {
            imports = with self.modules.homeManager; [
              emacs
              shell
            ];
          };
        }
      ];

    networking = {
      hostName = "beta";
      computerName = "beta";
    };
    system.defaults.smb.NetBIOSName = "beta";
  };
}
