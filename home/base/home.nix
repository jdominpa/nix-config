{myLib, ...}: {
  # Let home-manager manage itself
  programs.home-manager.enable = true;

  home = {
    inherit (myLib.vars) username;
    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    stateVersion = "24.05";
  };
}
