{
  inputs,
  self,
  ...
}:
let
  settings =
    { pkgs, ... }:
    {
      stylix = {
        enable = true;
        autoEnable = true;
        base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
        fonts = {
          sansSerif = {
            package = pkgs.aporetic;
            name = "Aporetic Sans";
          };
          serif = {
            package = pkgs.aporetic;
            name = "Aporetic Serif";
          };
          monospace = {
            package = pkgs.aporetic;
            name = "Aporetic Sans Mono";
          };
          emoji = {
            package = pkgs.noto-fonts-color-emoji;
            name = "Noto Color Emoji";
          };
          sizes = {
            applications = 12;
            desktop = 10;
            terminal = 13;
          };
        };
        image = null;
        imageScalingMode = "fill";
        opacity = {
          applications = 1.0;
          desktop = 0.5;
          popups = 1.0;
          terminal = 1.0;
        };
        polarity = "dark";
      };
      home-manager.sharedModules = [ self.modules.homeManager.stylix ];
    };
in
{
  flake.modules.nixos.stylix =
    { pkgs, ... }:
    {
      imports = [
        inputs.stylix.nixosModules.stylix
        settings
      ];
      stylix = {
        cursor = {
          name = "Bibata-Modern-Ice";
          package = pkgs.bibata-cursors;
          size = 24;
        };
        icons = {
          enable = true;
          dark = "Papirus-Dark";
          light = "Papirus-Light";
          package = pkgs.papirus-icon-theme;
        };
      };
    };

  flake.modules.darwin.stylix = {
    imports = [
      inputs.stylix.darwinModules.stylix
      settings
    ];
  };

  flake.modules.homeManager.stylix = {
    stylix.targets = {
      emacs.enable = false;
      kitty.colors.enable = false;
      noctalia-shell.enable = false;
      starship.colors.enable = false;
      zen-browser.profileNames = [ "default" ];
    };
  };
}
