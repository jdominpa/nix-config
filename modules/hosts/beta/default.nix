{
  inputs,
  self,
  ...
}:
let
  hostName = "beta";
in
{
  flake.darwinConfigurations.${hostName} = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    modules = [
      { nixpkgs.hostPlatform = inputs.nixpkgs.lib.mkDefault "aarch64-darwin"; }
      self.modules.darwin.${hostName}
    ];
  };

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
        kanata
        nix
        nix-index
        shell
        stylix
        syncthing
        terminal
      ]
      ++ [
        {
          home-manager.users.jdominpa = {
            imports = with self.modules.homeManager; [
              bitwarden
              emacs
              git
              latex
              syncthing
            ];
          };
        }
      ];

    networking = {
      inherit hostName;
      computerName = hostName;
    };
    system.defaults.smb.NetBIOSName = hostName;

    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    system.stateVersion = 6;
  };
}
