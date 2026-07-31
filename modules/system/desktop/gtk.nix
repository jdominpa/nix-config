{
  flake.nixosModules.gtk =
    { lib, pkgs, ... }:
    let
      theme = {
        gtk = {
          name = "Adwaita-dark";
          package = pkgs.adwaita-icon-theme;
        };
        icons = {
          name = "Papirus-Dark";
          package = pkgs.papirus-icon-theme;
        };
        cursor = {
          name = "Bibata-Modern-Ice";
          size = 24;
          package = pkgs.bibata-cursors;
        };
        font = "Aporetic Sans 11";
      };
      gtkSettings = ''
        [Settings]
        gtk-application-prefer-dark=1
        gtk-cursor-theme-name=${theme.cursor.name}
        gtk-cursor-theme-size=${lib.toString theme.cursor.size}
        gtk-font-name=${theme.font}
        gtk-icon-theme-name=${theme.icons.name}
        gtk-theme-name=${theme.gtk.name}
      '';
    in
    {
      environment = {
        systemPackages = [
          theme.cursor.package
          theme.gtk.package
          theme.icons.package
        ];
        etc = {
          "xdg/gtk-3.0/settings.ini".text = gtkSettings;
          "xdg/gtk-4.0/settings.ini".text = gtkSettings;
        };
      };
      programs.dconf = {
        enable = true;
        profiles.user.databases = [
          {
            settings."org/gnome/desktop/interface" = {
              color-scheme = "prefer-dark";
              cursor-size = lib.gvariant.mkUint16 theme.cursor.size;
              cursor-theme = theme.cursor.name;
              font-name = theme.font;
              gtk-theme = theme.gtk.name;
              icon-theme = theme.icons.name;
            };
          }
        ];
      };
    };
}
