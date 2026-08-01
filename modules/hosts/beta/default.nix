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
      self.darwinModules.${hostName}
    ];
  };

  flake.modules.darwin.beta = {
    imports = with self.darwinModules; [
      bitwarden
      brave
      cli-tools
      desktop
      emacs
      git
      kanata
      kitty
      latex
      syncthing
      zsh
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
