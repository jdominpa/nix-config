{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.system.settings;
in
{
  options.jdp.darwin = {
    system.settings.enable = mkEnableOption "Enable general macOS system settings.";
  };

  config = mkIf cfg.enable {
    system = {
      # activationScripts are executed every time you boot the system or run `nixos-rebuild` / `darwin-rebuild`.
      activationScripts.postUserActivation.text = ''
        # activateSettings -u will reload the settings from the database and apply them to the current session,
        # so we do not need to logout and login again to make the changes take effect.
        /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
      '';

      defaults = {
        LaunchServices = {
          LSQuarantine = false; # disable "Are you sure you want to open this application?" message
        };

        # macOS settings
        NSGlobalDomain = {
          # `defaults read NSGlobalDomain <key>`
          "com.apple.swipescrolldirection" = true; # enable natural scrolling(default to true)
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
          "com.apple.Safari" = {
            # Disable opening 'safe' files automatically
            AutoOpenSafeDownloads = true;
            # Use advanced tracking and fingerprinting protection in all browsing
            EnableEnhancedPrivacyInPrivateBrowsing = true;
            EnableEnhancedPrivacyInRegularBrowsing = true;
            # Set backspace key to go to the previous page in history
            "com.apple.Safari.ContentPageGroupIdentifier.WebKit2BackspaceKeyNavigationEnabled" = true;
            # Enable the 'Develop' menu and the 'Web Inspector'
            "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" = true;
            IncludeDevelopMenu = true;
            WebKitDeveloperExtrasEnabledPreferenceKey = true;
            # Set search type to 'Contains' instead of 'Starts With'
            FindOnPageMatchesWordStartsOnly = true;
            # Enable debug menu
            IncludeInternalDebugMenu = true;
            # Update extensions automatically
            InstallExtensionUpdatesAutomatically = true;
            # Enable 'Do Not Track'
            SendDoNotTrackHTTPHeader = true;
            # Don’t send search queries to Apple
            SuppressSearchSuggestions = true;
            UniversalSearchEnabled = true;
            # Warn about fraudulent websites
            WarnAboutFraudulentWebsites = true;
            # Add a context menu item for showing the 'Web Inspector' in web views
            WebKitDeveloperExtras = true;
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
          "com.apple.terminal" = {
            # Enable 'Secure Keyboard Entry'
            SecureKeyboardEntry = true;
          };
        };
      };
    };
  };
}
