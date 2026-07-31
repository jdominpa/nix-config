{
  flake.modules.darwin.base-settings = {
    security.pam.services.sudo_local.touchIdAuth = true;
    system.defaults = {
      finder = {
        AppleShowAllExtensions = true; # show file extensions
        FXDefaultSearchScope = "SCcf";
        FXEnableExtensionChangeWarning = false; # don't warn when changing file extensions
        FXPreferredViewStyle = "clmv"; # set column view as default view style
        _FXSortFoldersFirst = true; # keep folders on top
        _FXSortFoldersFirstOnDesktop = true;
        _FXShowPosixPathInTitle = true;
        FXRemoveOldTrashItems = true;
        NewWindowTarget = "Home";
        ShowExternalHardDrivesOnDesktop = true;
        ShowHardDrivesOnDesktop = true;
        ShowMountedServersOnDesktop = true;
        ShowRemovableMediaOnDesktop = true;
        ShowPathbar = true;
        ShowStatusBar = true;
      };

      LaunchServices = {
        LSQuarantine = false; # disable "Are you sure you want to open this application?" message
      };

      # macOS settings
      NSGlobalDomain = {
        # `defaults read NSGlobalDomain <key>`
        "com.apple.swipescrolldirection" = false; # enable natural scrolling(default to true)
        AppleFontSmoothing = 1;
        AppleInterfaceStyle = "Dark"; # dark mode
        AppleMeasurementUnits = "Centimeters";
        AppleMetricUnits = 1;
        NSAutomaticCapitalizationEnabled = false; # disable auto capitalization
        NSAutomaticDashSubstitutionEnabled = false; # disable auto dash substitution
        NSAutomaticPeriodSubstitutionEnabled = false; # disable auto period substitution
        NSAutomaticQuoteSubstitutionEnabled = false; # disable auto quote substitution
        NSAutomaticSpellingCorrectionEnabled = false; # disable auto spelling correction
        NSNavPanelExpandedStateForSaveMode = true; # expand save panel by default
        NSNavPanelExpandedStateForSaveMode2 = true;
        PMPrintingExpandedStateForPrint = true; # expand print panel by default
        PMPrintingExpandedStateForPrint2 = true;
        NSDocumentSaveNewDocumentsToCloud = false; # save to disk, not iCloud
      };

      # Customize settings that aren't supported by nix-darwin directly
      # Incomplete list of macOS `defaults` commands :
      #   https://github.com/yannbertrand/macos-defaults
      CustomUserPreferences = {
        ".GlobalPreferences" = {
          # Automatically switch to a new space when switching to the application
          AppleSpacesSwitchOnActivate = true;
        };
        NSGlobalDomain = {
          # Add a context menu item for showing the Web Inspector in web views
          WebKitDeveloperExtras = true;
        };
        "com.apple.appstore" = {
          # Enable debug menu in the app store
          ShowDebugMenu = true;
        };
        "com.apple.commerce" = {
          # Turn on automatic app updates
          AutoUpdate = true;
        };
        "com.apple.desktopservices" = {
          # Avoid creating .DS_Store files on network or USB volumes
          DSDontWriteNetworkStores = true;
          DSDontWriteUSBStores = true;
        };
        "com.apple.finder" = {
          ShowRecentTags = false; # don't show recent tags
          WarnOnEmptyTrash = false; # disable the warning before emptying the trash
        };
        "com.apple.ImageCapture" = {
          # Prevent Photos from opening automatically when devices are plugged in
          disableHotPlug = true;
        };
        "com.apple.screensaver" = {
          # Require password immediately after sleep or screen saver begins
          askForPassword = 1;
          askForPasswordDelay = 0;
        };
        "com.apple.screencapture" = {
          location = "~/Desktop";
          show-thumbnail = false;
          type = "png";
        };
        "com.apple.SoftwareUpdate" = {
          AutomaticCheckEnabled = true; # automatic update check
          AutomaticDownload = 1; # download new updates in the background
          CriticalUpdateInstall = 1; # install system data files & security updates
        };
      };
    };
  };
}
