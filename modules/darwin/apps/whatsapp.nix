{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.darwin.apps.whatsapp;
in
{
  options.jdp.darwin = {
    apps.whatsapp.enable = lib.mkEnableOption "Install WhatsApp through homebrew.";
  };

  config = lib.mkIf cfg.enable {
    homebrew.masApps = {
      WhatsApp = 310633997;
    };
  };
}
