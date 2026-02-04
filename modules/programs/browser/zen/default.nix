{
  inputs,
  self,
  ...
}:
{
  flake.modules.nixos.zen-browser = {
    home-manager.sharedModules = [ self.modules.homeManager.zen-browser ];
  };

  flake.modules.darwin.zen-browser = {
    home-manager.sharedModules = [ self.modules.homeManager.zen-browser ];
  };

  flake.modules.homeManager.zen-browser =
    { pkgs, ... }:
    {
      imports = [
        inputs.zen-browser.homeModules.twilight
        self.modules.homeManager.zen-browser-policies
        self.modules.homeManager.zen-browser-search
        self.modules.homeManager.zen-browser-settings
      ];
      programs.zen-browser = {
        enable = true;
        nativeMessagingHosts = [ pkgs.firefoxpwa ];
        profiles.default = {
          id = 0;
          name = "Default";
          isDefault = true;
        };
      };
    };
}
