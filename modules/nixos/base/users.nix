{
  pkgs,
  myLib,
  ...
}: {
  users.users.${myLib.vars.username} = {
    isNormalUser = true;
    home = "/home/${myLib.vars.username}";
    shell = pkgs.zsh;
    extraGroups = ["networkmanager" "wheel"];
  };
}
