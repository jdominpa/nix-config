{myLib, ...}: {
  home.homeDirectory = "/home/${myLib.vars.username}";
}
