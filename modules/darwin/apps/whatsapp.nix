{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.darwin.apps.whatsapp;
in
{
  options.jdp.darwin = {
    apps.whatsapp.enable = mkEnableOption "Install WhatsApp through homebrew.";
  };

  config = mkIf cfg.enable {
    homebrew.masApps = {
      WhatsApp = 310633997;
    };
  };
}
