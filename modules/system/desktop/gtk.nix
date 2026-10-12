{
  flake.modules.nixos.gtk =
    { lib, pkgs, ... }:
    let
      theme = {
        gtk = {
          name = "adw-gtk3-dark";
          package = pkgs.adw-gtk3;
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
        font = "Aporetic Sans 12";
      };
      # Ensure xwayland-satellite uses the cursor theme
      defaultCursorTheme = pkgs.writeTextDir "share/icons/default/index.theme" ''
        [Icon Theme]
        Name=default
        Comment=Alias for ${theme.cursor.name}
        Inherits=${theme.cursor.name}
      '';
      gtkSettings = ''
        [Settings]
        gtk-application-prefer-dark-theme=1
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
          defaultCursorTheme
          theme.cursor.package
          theme.gtk.package
          theme.icons.package
          # Papirus inherits from Adwaita, the base set has to be present
          pkgs.adwaita-icon-theme
        ];
        etc = {
          "xdg/gtk-3.0/settings.ini".text = gtkSettings;
          "xdg/gtk-4.0/settings.ini".text = gtkSettings;
        };
        # Set X cursor theme
        sessionVariables = {
          XCURSOR_SIZE = lib.toString theme.cursor.size;
          XCURSOR_THEME = theme.cursor.name;
        };
      };
      programs.dconf = {
        enable = true;
        profiles.user.databases = [
          {
            settings."org/gnome/desktop/interface" = {
              color-scheme = "prefer-dark";
              cursor-size = lib.gvariant.mkInt32 theme.cursor.size;
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
