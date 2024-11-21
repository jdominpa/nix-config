{
  security.pam.enableSudoTouchIdAuth = true;

  time.timeZone = "Europe/Madrid";

  system = {
    # activationScripts are executed every time you boot the system or run `nixos-rebuild` / `darwin-rebuild`.
    activationScripts.postUserActivation.text = ''
      # activateSettings -u will reload the settings from the database and apply them to the current session,
      # so we do not need to logout and login again to make the changes take effect.
      /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
    '';

    defaults = {
      menuExtraClock.Show24Hour = true; # show 24 hour format

      # Dock settings
      dock = {
        autohide = true;
        show-recents = false;
        mru-spaces = false;
        expose-group-by-app = true; # group windows by application
      };

      # Finder settings
      finder = {
        AppleShowAllExtensions = true; # show file extensions
        FXEnableExtensionChangeWarning = false; # don't warn when changing file extensions
        FXPreferredViewStyle = "Nlsv"; # set list as default view style
        _FXSortFoldersFirst = true; # keep folders on top
        FXRemoveOldTrashItems = true;
        ShowExternalHardDrivesOnDesktop = true;
        ShowHardDrivesOnDesktop = true;
        ShowMountedServersOnDesktop = true;
        ShowRemovableMediaOnDesktop = true;
        ShowPathbar = true;
        ShowStatusBar = true;
      };

      # Trackpad
      trackpad = {
        Clicking = true; # enable tap to click
        TrackpadRightClick = true; # enable two finger right click
        TrackpadThreeFingerDrag = true; # enable three finger drag
      };

      # macOS settings
      NSGlobalDomain = {
        # `defaults read NSGlobalDomain <key>`
        "com.apple.swipescrolldirection" = true; # enable natural scrolling(default to true)
        "com.apple.sound.beep.feedback" = 0; # disable beep sound when pressing volume up/down key
        AppleMeasurementUnits = "Centimeters";
        AppleMetricUnits = 1;
        AppleInterfaceStyle = "Dark"; # dark mode
        AppleShowScrollBars = "Always";
        AppleKeyboardUIMode = 3; # Mode 3 enables full keyboard control.
        ApplePressAndHoldEnabled = true; # enable press and hold
        InitialKeyRepeat = 12; # normal minimum is 15 (225 ms), maximum is 120 (1800 ms)
        KeyRepeat = 2; # normal minimum is 2 (30 ms), maximum is 120 (1800 ms)
        NSAutomaticCapitalizationEnabled = false; # disable auto capitalization
        NSAutomaticDashSubstitutionEnabled = false; # disable auto dash substitution
        NSAutomaticPeriodSubstitutionEnabled = false; # disable auto period substitution
        NSAutomaticQuoteSubstitutionEnabled = false; # disable auto quote substitution
        NSAutomaticSpellingCorrectionEnabled = false; # disable auto spelling correction
        NSNavPanelExpandedStateForSaveMode = true; # expand save panel by default
        NSNavPanelExpandedStateForSaveMode2 = true;
        PMPrintingExpandedStateForPrint = true; # expand print panel by default
        PMPrintingExpandedStateForPrint2 = true;
      };

      # customize settings that not supported by nix-darwin directly
      # Incomplete list of macOS `defaults` commands :
      #   https://github.com/yannbertrand/macos-defaults
      CustomUserPreferences = {
        ".GlobalPreferences" = {
          # automatically switch to a new space when switching to the application
          AppleSpacesSwitchOnActivate = true;
        };
        NSGlobalDomain = {
          # Add a context menu item for showing the Web Inspector in web views
          WebKitDeveloperExtras = true;
        };
        "com.apple.finder" = {
          # AppleShowAllFiles = true;
          # _FXSortFoldersFirst = true;
          # When performing a search, search the current folder by default
          FXDefaultSearchScope = "SCcf";
        };
        "com.apple.desktopservices" = {
          # Avoid creating .DS_Store files on network or USB volumes
          DSDontWriteNetworkStores = true;
          DSDontWriteUSBStores = true;
        };
        # "com.apple.WindowManager" = {
        #   EnableStandardClickToShowDesktop = 0; # Click wallpaper to reveal desktop
        #   StandardHideDesktopIcons = 0; # Show items on desktop
        #   HideDesktop = 0; # Do not hide items on desktop & stage manager
        #   StageManagerHideWidgets = 0;
        #   StandardHideWidgets = 0;
        # };
        "com.apple.screensaver" = {
          # Require password immediately after sleep or screen saver begins
          askForPassword = 1;
          askForPasswordDelay = 0;
        };
        "com.apple.screencapture" = {
          location = "~/Desktop";
          type = "png";
        };
        # Prevent Photos from opening automatically when devices are plugged in
        "com.apple.ImageCapture".disableHotPlug = true;
      };

      loginwindow = {
        GuestEnabled = false; # disable guest user
        SHOWFULLNAME = true; # show full name in login window
      };
    };

    # Remap capslock to escape
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
    };
  };

  fonts.packages = with pkgs; [
    stable.iosevka-comfy.comfy
    font-awesome
    (nerdfonts.override {
      fonts = ["Iosevka"];
    })
  ];
}
