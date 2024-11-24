{
  config,
  lib,
  myLib,
  ...
}: {
  options = {
    jdp.home.homeDirectory = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Sets the home directory path for home-manager.";
      example = "/home/foo";
    };
  };

  config = {
    # Let home-manager manage itself
    programs.home-manager.enable = true;

    home = {
      inherit (myLib.vars) username;
      homeDirectory = config.jdp.home.homeDirectory;
      # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
      stateVersion = "24.05";
    };
  };
}
