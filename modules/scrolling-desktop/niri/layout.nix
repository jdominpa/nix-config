{
  flake.modules.homeManager.niri = {
    programs.niri.settings.layout = {
      border.enable = false;
      focus-ring = {
        enable = true;
        active = {
          color = "#2fafff";
        };
        inactive = {
          color = "transparent";
        };
        urgent = {
          color = "#ff5f59";
        };
        width = 2;
      };
      gaps = 12;
      shadow.enable = true;
      struts = {
        left = 32;
        right = 32;
      };
    };
  };
}
