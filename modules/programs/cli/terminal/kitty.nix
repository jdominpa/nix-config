{
  self,
  ...
}:
{
  flake.modules.nixos.terminal =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.kitty ];
      home-manager.sharedModules = [ self.modules.homeManager.terminal ];
    };

  flake.modules.darwin.terminal =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.kitty ];
      home-manager.sharedModules = [ self.modules.homeManager.terminal ];
    };

  flake.modules.homeManager.terminal = {
    programs.kitty = {
      enable = true;
      settings = {
        cursor_shape = "block";
        cursor_blink_interval = 0;
        tab_bar_edge = "top";
        tab_bar_style = "slant";
      };
      themeFile = "Modus_Vivendi";
    };
  };
}
