{
  inputs,
  config,
  lib,
  ...
}: {
  imports = [
    inputs.plasma-manager.homeManagerModules.plasma-manager
    ./input.nix
    ./keybinds.nix
    ./konsole.nix
    ./kwin.nix
    ./panels.nix
    ./theme.nix
  ];

  options = {
    plasma-manager.enable = lib.mkEnableOption "plasma-manager";
  };

  config = lib.mkIf config.plasma-manager.enable {
    programs.plasma = {
      enable = true;
      overrideConfig = true;
      immutableByDefault = true;
      krunner.position = "center";
      configFile = {
        "baloofilerc"."General"."exclude filters version" = 9;
        "dolphinrc"."KFileDialog Settings"."Places Icons Auto-resize" = false;
        "dolphinrc"."KFileDialog Settings"."Places Icons Static Size" = 22;
        # "kcminputrc"."Libinput/1133/49291/Logitech G502 HERO Gaming Mouse"."PointerAccelerationProfile" = 1;
        "kcminputrc"."Mouse"."cursorTheme" = "breeze_cursors";
        "kded5rc"."Module-browserintegrationreminder"."autoload" = false;
        "kded5rc"."Module-device_automounter"."autoload" = false;
        "kdeglobals"."KFileDialog Settings"."Allow Expansion" = false;
        "kdeglobals"."KFileDialog Settings"."Automatically select filename extension" = true;
        "kdeglobals"."KFileDialog Settings"."Breadcrumb Navigation" = true;
        "kdeglobals"."KFileDialog Settings"."Decoration position" = 2;
        "kdeglobals"."KFileDialog Settings"."LocationCombo Completionmode" = 5;
        "kdeglobals"."KFileDialog Settings"."PathCombo Completionmode" = 5;
        "kdeglobals"."KFileDialog Settings"."Show Bookmarks" = false;
        "kdeglobals"."KFileDialog Settings"."Show Full Path" = false;
        "kdeglobals"."KFileDialog Settings"."Show Inline Previews" = true;
        "kdeglobals"."KFileDialog Settings"."Show Preview" = false;
        "kdeglobals"."KFileDialog Settings"."Show Speedbar" = true;
        "kdeglobals"."KFileDialog Settings"."Show hidden files" = false;
        "kdeglobals"."KFileDialog Settings"."Sort by" = "Name";
        "kdeglobals"."KFileDialog Settings"."Sort directories first" = true;
        "kdeglobals"."KFileDialog Settings"."Sort hidden files last" = false;
        "kdeglobals"."KFileDialog Settings"."Sort reversed" = false;
        "kdeglobals"."KFileDialog Settings"."Speedbar Width" = 140;
        "kdeglobals"."KFileDialog Settings"."View Style" = "DetailTree";
        "kiorc"."Confirmations"."ConfirmEmptyTrash" = true;
        "klipperrc"."General"."IgnoreImages" = false;
        "klipperrc"."General"."KeepClipboardContents" = false;
        "kscreenlockerrc"."Daemon"."Autolock" = false;
        "kscreenlockerrc"."Daemon"."LockGrace" = 0;
        "kwalletrc"."Wallet"."Enabled" = false;
        "kwalletrc"."org.freedesktop.secrets"."apiEnabled" = true;
        "kwinrc"."Xwayland"."Scale" = 1;
        "plasmaparc"."General"."RaiseMaximumVolume" = true;
        "plasmaparc"."General"."VolumeStep" = 2;
      };
    };
  };
}
