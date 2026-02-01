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
        desktop-system
        emacs
        homebrew
        home-manager
        jdominpa
        nix
        shell
        stylix
        terminal
      ]
      ++ [
        {
          home-manager.users.jdominpa = {
            imports = with self.modules.homeManager; [
              bitwarden
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
