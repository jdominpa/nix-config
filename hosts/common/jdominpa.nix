{
  pkgs,
  config,
  ...
}: {
  users.users.jdominpa = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = ["networkmanager" "wheel"];
    packages = [pkgs.home-manager];
  };

  home-manager.users.jdominpa = import ../../home-manager/${config.networking.hostName};
}
