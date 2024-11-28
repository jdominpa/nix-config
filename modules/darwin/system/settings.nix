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
          "com.apple.sound.beep.feedback" = 0; # disable beep sound when pressing volume up/down key
          AppleMeasurementUnits = "Centimeters";
          AppleMetricUnits = 1;
          AppleInterfaceStyle = "Dark"; # dark mode
          AppleShowScrollBars = "Always";
          AppleFontSmoothing = 1;
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
          "com.apple.desktopservices" = {
            # Avoid creating .DS_Store files on network or USB volumes
            DSDontWriteNetworkStores = true;
            DSDontWriteUSBStores = true;
          };
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
      };
    };
  };
}
