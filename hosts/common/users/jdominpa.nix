{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];
  
  options = {
    jdominpa.enable = lib.mkEnableOption "Create and configure user jdominpa.";
  };

  config = lib.mkIf config.jdominpa.enable {
    users.users.jdominpa = {
      isNormalUser = true;
      shell = pkgs.zsh;
      extraGroups = ["networkmanager" "wheel"];
      packages = [pkgs.home-manager];
    };

    home-manager.users.jdominpa = import ../../../home-manager/${config.networking.hostName};
  };
}
