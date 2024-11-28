{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.apps.finder;
in
{
  options.jdp.darwin = {
    apps.finder.enable = mkEnableOption "Configure Finder app.";
  };

  config = mkIf cfg.enable {
    system.defaults = {
      finder = {
        AppleShowAllExtensions = true; # show file extensions
        FXEnableExtensionChangeWarning = false; # don't warn when changing file extensions
        FXPreferredViewStyle = "Nlsv"; # set list as default view style
        _FXSortFoldersFirst = true; # keep folders on top
        FXDefaultSearchScope = "SCcf";
        FXRemoveOldTrashItems = true;
        ShowExternalHardDrivesOnDesktop = true;
        ShowHardDrivesOnDesktop = true;
        ShowMountedServersOnDesktop = true;
        ShowRemovableMediaOnDesktop = true;
        ShowPathbar = true;
        ShowStatusBar = true;
      };
      CustomUserPreferences = {
        "com.apple.finder" = {
          # When performing a search, search the current folder by default
          FXDefaultSearchScope = "SCcf";
        };
      };
    };
  };
}
