{
  self,
  ...
}:
let
  username = "jdominpa";
  sharedSettings =
    { pkgs, ... }:
    {
      nix.settings.trusted-users = [ username ];
      users.users.${username}.shell = pkgs.zsh;
      home-manager.users.${username} = {
        imports = [ self.modules.homeManager.${username} ];
      };
    };
in
{
  flake.modules.nixos.${username} = {
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

  flake.modules.darwin.${username} = {
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
      # Let home-manager manage itself
      programs.home-manager.enable = true;
      home = {
        inherit username;
        homeDirectory = (if isLinux then "/home" else "/Users") + "/${username}";
        # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
        stateVersion = "24.05";
      };
    };
}
