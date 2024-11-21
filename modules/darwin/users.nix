{
  pkgs,
  myLib,
  ...
}: {
  users.users.${myLib.vars.username} = {
    home = "/Users/${myLib.vars.username}";
    shell = pkgs.zsh;
  };
}
