{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.apps.finder;
in
{
  options.jdp.darwin = {
    apps.finder.enable = lib.mkEnableOption "Configure Finder app.";
  };

  config = lib.mkIf cfg.enable {
    system.defaults = {
      finder = {
        AppleShowAllExtensions = true; # show file extensions
        FXDefaultSearchScope = "SCcf";
        FXEnableExtensionChangeWarning = false; # don't warn when changing file extensions
        FXPreferredViewStyle = "Nlsv"; # set list as default view style
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
    };
  };
}
