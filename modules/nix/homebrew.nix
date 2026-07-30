{
  flake.darwinModules.homebrew = {
    homebrew = {
      enable = true;
      onActivation = {
        cleanup = "zap";
      };
    };
  };
}
