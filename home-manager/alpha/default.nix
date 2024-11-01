{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ../features
  ];

  plasma-manager.enable = true;
  programs.plasma.input = {
    mice = [
      {
        name = "Logitech G502 HERO Gaming Mouse";
        enable = true;
        accelerationProfile = "none";
        productId = "c08b";
        vendorId = "046d";
      }
    ];
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "24.05";
}
