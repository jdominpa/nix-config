{pkgs, ...}: {
  fonts = {
    enableDefaultPackages = false;
    fontDir.enable = true;
    packages = with pkgs; [
      stable.iosevka-comfy.comfy
      font-awesome
      noto-fonts-emoji
      (nerdfonts.override {
        fonts = ["Iosevka"];
      })
    ];

    # User defined fonts
    fontconfig.defaultFonts = {
      monospace = ["Iosevka Comfy" "Noto Color Emoji"];
      emoji = ["Noto Color Emoji"];
    };
  };
}
