{
  flake.modules.homeManager.zen-browser-policies =
    let
      mkLockedAttrs = builtins.mapAttrs (
        _: value: {
          Value = value;
          Status = "locked";
        }
      );

      mkExtensionEntry =
        {
          id,
          pinned ? false,
        }:
        let
          base = {
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";
            installation_mode = "force_installed";
          };
        in
        if pinned then base // { default_area = "navbar"; } else base;

      mkExtensionSettings = builtins.mapAttrs (
        _: entry: if builtins.isAttrs entry then entry else mkExtensionEntry { id = entry; }
      );
    in
    {
      programs.zen-browser.policies = {
        AutofillAddressEnabled = true;
        AutofillCreditCardEnabled = false;
        DisableAppUpdate = true;
        DisableFeedbackCommands = true;
        DisableFirefoxStudies = true;
        DisableMasterPasswordCreation = true;
        DisablePocket = true;
        DisableTelemetry = true;
        DontCheckDefaultBrowser = true;
        EnableTrackingProtection = {
          Value = true;
          Locked = true;
          Cryptomining = true;
          Fingerprinting = true;
        };
        ExtensionSettings = mkExtensionSettings {
          "uBlock0@raymondhill.net" = mkExtensionEntry {
            id = "ublock-origin";
            pinned = true;
          };
          "{446900e4-71c2-419f-a6a7-df9c091e268b}" = mkExtensionEntry {
            id = "bitwarden-password-manager";
            pinned = true;
          };
          "addon@darkreader.org" = mkExtensionEntry {
            id = "darkreader";
            pinned = true;
          };
        };
        HardwareAcceleration = true;
        NoDefaultBookmarks = true;
        OfferToSaveLogins = false;
        PasswordManagerEnabled = false;
        Preferences = mkLockedAttrs {
          "browser.aboutConfig.showWarning" = false;
          "browser.tabs.warnOnClose" = false;
          "browser.tabs.hoverPreview.enabled" = true;
          "gfx.webrender.all" = true;
          "privacy.resistFingerprinting" = true;
          "privacy.resistFingerprinting.randomization.canvas.use_siphash" = true;
          "privacy.resistFingerprinting.randomization.daily_reset.enabled" = true;
          "privacy.resistFingerprinting.randomization.daily_reset.private.enabled" = true;
          "privacy.resistFingerprinting.block_mozAddonManager" = true;
          "privacy.spoof_english" = 1;
        };
        SanitizeOnShutdown = {
          Cache = true;
          FormData = true;
          Locked = true;
        };
      };
    };
}
