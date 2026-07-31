{
  self,
  ...
}:
let
  username = "jdominpa";
  sharedSettings =
    { config, ... }:
    {
      nix.settings.trusted-users = [ username ];
      # `wrappers.zsh` is declared by the zsh module's install module, which
      # every host imports alongside this one.
      users.users.${username}.shell = config.wrappers.zsh.wrapper;
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
      initialPassword = "1234";
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
      programs.home-manager.enable = true; # let home-manager manage itself
      home = {
        inherit username;
        homeDirectory = (if isLinux then "/home" else "/Users") + "/${username}";
        # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
        stateVersion = "24.05";
      };
    };
}
