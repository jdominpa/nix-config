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
        "dolphinrc"."General"."ViewPropsTimestamp" = "2024,10,25,17,55,26.751";
        "dolphinrc"."KFileDialog Settings"."Places Icons Auto-resize" = false;
        "dolphinrc"."KFileDialog Settings"."Places Icons Static Size" = 22;
        "kactivitymanagerdrc"."activities"."1c37765e-278f-4d74-a293-438b6df2b3c9" = "Default";
        "kactivitymanagerdrc"."main"."currentActivity" = "1c37765e-278f-4d74-a293-438b6df2b3c9";
        "kcminputrc"."Libinput/1133/49291/Logitech G502 HERO Gaming Mouse"."PointerAccelerationProfile" = 1;
        "kcminputrc"."Mouse"."cursorTheme" = "breeze_cursors";
        "kded5rc"."Module-browserintegrationreminder"."autoload" = false;
        "kded5rc"."Module-device_automounter"."autoload" = false;
        "kdeglobals"."DirSelect Dialog"."DirSelectDialog Size" = "820,584";
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
        "kdeglobals"."WM"."activeBackground" = "49,54,59";
        "kdeglobals"."WM"."activeBlend" = "252,252,252";
        "kdeglobals"."WM"."activeForeground" = "252,252,252";
        "kdeglobals"."WM"."inactiveBackground" = "42,46,50";
        "kdeglobals"."WM"."inactiveBlend" = "161,169,177";
        "kdeglobals"."WM"."inactiveForeground" = "161,169,177";
        "kiorc"."Confirmations"."ConfirmEmptyTrash" = true;
        "klipperrc"."General"."IgnoreImages" = false;
        "klipperrc"."General"."KeepClipboardContents" = false;
        "kscreenlockerrc"."Daemon"."Autolock" = false;
        "kscreenlockerrc"."Daemon"."LockGrace" = 0;
        "kwalletrc"."Wallet"."Close When Idle" = false;
        "kwalletrc"."Wallet"."Close on Screensaver" = false;
        "kwalletrc"."Wallet"."Default Wallet" = "kdewallet";
        "kwalletrc"."Wallet"."Enabled" = false;
        "kwalletrc"."Wallet"."Idle Timeout" = 10;
        "kwalletrc"."Wallet"."Launch Manager" = false;
        "kwalletrc"."Wallet"."Leave Manager Open" = false;
        "kwalletrc"."Wallet"."Leave Open" = true;
        "kwalletrc"."Wallet"."Prompt on Open" = false;
        "kwalletrc"."Wallet"."Use One Wallet" = true;
        "kwalletrc"."org.freedesktop.secrets"."apiEnabled" = true;
        "kwinrc"."Desktops"."Id_1" = "6398b22a-02d1-493d-9980-76dc342f24fd";
        "kwinrc"."Desktops"."Number" = 1;
        "kwinrc"."Desktops"."Rows" = 1;
        "kwinrc"."NightColor"."Active" = true;
        "kwinrc"."NightColor"."NightTemperature" = 3500;
        "kwinrc"."Tiling"."padding" = 4;
        "kwinrc"."Tiling/8806a668-755d-5fb2-af61-53fba26612b2"."tiles" = "{\"layoutDirection\":\"horizontal\",\"tiles\":[{\"width\":0.25},{\"width\":0.5},{\"width\":0.25}]}";
        "kwinrc"."Tiling/d02c035d-eef0-5c68-aa2f-2e83d0ba08b1"."tiles" = "{\"layoutDirection\":\"horizontal\",\"tiles\":[{\"width\":0.25},{\"width\":0.5},{\"width\":0.25}]}";
        "kwinrc"."Tiling/d8c2c5a5-ad69-5017-91a6-022a1909dbbe"."tiles" = "{\"layoutDirection\":\"horizontal\",\"tiles\":[{\"width\":0.25},{\"width\":0.5},{\"width\":0.25}]}";
        "kwinrc"."Xwayland"."Scale" = 1;
        "kwinrc"."org.kde.kdecoration2"."BorderSize" = "None";
        "kwinrc"."org.kde.kdecoration2"."BorderSizeAuto" = false;
        "kwinrc"."org.kde.kdecoration2"."theme" = "Breeze";
        "kxkbrc"."Layout"."DisplayNames" = ",";
        "kxkbrc"."Layout"."LayoutList" = "us,us";
        "kxkbrc"."Layout"."ResetOldOptions" = true;
        "kxkbrc"."Layout"."Use" = true;
        "kxkbrc"."Layout"."VariantList" = ",intl";
        "plasma-localerc"."Formats"."LANG" = "en_GB.UTF-8";
        "plasma-localerc"."Formats"."LC_MEASUREMENT" = "ca_ES.UTF-8";
        "plasma-localerc"."Formats"."LC_MONETARY" = "ca_ES.UTF-8";
        "plasmanotifyrc"."Applications/discord"."Seen" = true;
        "plasmaparc"."General"."RaiseMaximumVolume" = true;
        "plasmaparc"."General"."VolumeStep" = 2;
      };
    };
  };
}
