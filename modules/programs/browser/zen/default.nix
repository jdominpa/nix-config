{
  inputs,
  self,
  ...
}:
let
  zen-browser =
    { pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
    in
    {
      environment.systemPackages = [ inputs.zen-browser.packages."${system}".twilight ];
      home-manager.sharedModules = [ self.modules.homeManager.zen-browser ];
    };
in
{
  flake.modules.nixos.zen-browser = {
    imports = [ zen-browser ];
  };

  flake.modules.darwin.zen-browser = {
    imports = [ zen-browser ];
  };

  flake.modules.homeManager.zen-browser =
    { lib, pkgs, ... }:
    {
      imports = [
        inputs.zen-browser.homeModules.twilight
        self.modules.homeManager.zen-browser-policies
        self.modules.homeManager.zen-browser-search
        self.modules.homeManager.zen-browser-settings
      ];
      programs.zen-browser = {
        enable = true;
        nativeMessagingHosts = lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.firefoxpwa ];
        profiles.default = {
          id = 0;
          name = "Default";
          isDefault = true;
        };
      };
    };
}
