{
  flake.modules.darwin.homebrew = {
    homebrew = {
      enable = true;
      onActivation = {
        cleanup = "zap";
      };
    };
  };
}
