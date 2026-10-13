{
  flake.wrappers.niri.settings.layout = {
    background-color = "transparent"; # needed for overview mode with noctalia
    border.width = 2;
    focus-ring.off = _: { };
    gaps = 0;
    preset-column-widths = [
      { proportion = 1.0 / 3.0; }
      { proportion = 1.0 / 2.0; }
      { proportion = 2.0 / 3.0; }
    ];
    shadow.on = _: { };
    struts = {
      left = 0;
      right = 0;
    };
  };
}
