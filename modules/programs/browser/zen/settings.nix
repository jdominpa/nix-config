{
  flake.modules.homeManager.zen-browser-settings = {
    programs.zen-browser.profiles.default.settings = {
      "browser.aboutwelcome.enabled" = false;
      "zen.urlbar.behavior" = "float";
      "zen.urlbar.replace-newtab" = true;
      "zen.view.compact.hide-tabbar" = true;
      "zen.view.compact.hide-toolbar" = true;
      "zen.view.sidebar-expanded" = true;
      "zen.view.use-single-toolbar" = true;
      "zen.welcome-screen.seen" = true;
      "zen.workspaces.continue-where-left-off" = true;
    };
  };
}
