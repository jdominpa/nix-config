{
  settings.layout = {
    background-color = "transparent"; # needed for overview mode with noctalia
    border.off = _: { };
    focus-ring = {
      active-color = "#2fafff";
      inactive-color = "transparent";
      urgent-color = "#ff5f59";
      width = 2;
    };
    gaps = 12;
    preset-column-widths = [
      { proportion = 1.0 / 3.0; }
      { proportion = 1.0 / 2.0; }
      { proportion = 2.0 / 3.0; }
    ];
    shadow.on = _: { };
    struts = {
      left = 32;
      right = 32;
    };
  };
}
