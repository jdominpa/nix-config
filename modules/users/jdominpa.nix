{
  inputs,
  ...
}:
let
  sharedSettings =
    { pkgs, ... }:
    {
      nix.settings.trusted-users = [ "jdominpa" ];
      users.users.jdominpa.shell = pkgs.zsh;
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.jdominpa = {
          # Let home-manager manage itself
          programs.home-manager.enable = true;
          home = {
            username = "jdominpa";
            # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
            stateVersion = "24.05";
          };
        };
      };
    };
in
{
  flake.modules.nixos.jdominpa = {
    imports = [
      sharedSettings
      inputs.home-manager.nixosModules.home-manager
    ];
    users.users.jdominpa = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      home = "/home/jdominpa";
    };
    home-manager.users.jdominpa.home.homeDirectory = "/home/jdominpa";
  };

  flake.modules.darwin.jdominpa = {
    imports = [
      sharedSettings
      inputs.home-manager.darwinModules.home-manager
    ];
    users.users.jdominpa = {
      isHidden = false;
      home = "/Users/jdominpa";
    };
    system.primaryUser = "jdominpa";
    home-manager.users.jdominpa.home.homeDirectory = "/Users/jdominpa";
  };
}
