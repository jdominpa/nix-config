{
  moduleWithSystem,
  self,
  ...
}:
let
  username = "jdominpa";
  sharedSettings = moduleWithSystem (
    { self', ... }:
    {
      nix.settings.trusted-users = [ username ];
      users.users.${username}.shell = self'.packages.zsh;
      home-manager.users.${username} = {
        imports = [ self.modules.homeManager.${username} ];
      };
    }
  );
in
{
  flake.nixosModules.${username} = {
    imports = [ sharedSettings ];
    users.users.${username} = {
      isNormalUser = true;
      extraGroups = [
        "networkmanager"
        "wheel"
      ];
      home = "/home/${username}";
    };
  };

  flake.darwinModules.${username} = {
    imports = [ sharedSettings ];
    users.users.${username} = {
      isHidden = false;
      home = "/Users/${username}";
    };
    system.primaryUser = username;
  };

  flake.modules.homeManager.${username} =
    { pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isLinux;
    in
    {
      programs.home-manager.enable = true; # let home-manager manage itself
      home = {
        inherit username;
        homeDirectory = (if isLinux then "/home" else "/Users") + "/${username}";
        # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
        stateVersion = "24.05";
      };
    };
}
