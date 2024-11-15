{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    jdominpa.enable = lib.mkEnableOption "Create and configure user jdominpa.";
  };

  config = lib.mkIf config.jdominpa.enable {
    users.users.jdominpa = {
      isNormalUser = true;
      shell = pkgs.zsh;
      extraGroups = ["networkmanager" "wheel"];
    };
  };
}
