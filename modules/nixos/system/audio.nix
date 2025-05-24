{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.system.pipewire;
in
{
  options.jdp.nixos = {
    system.pipewire.enable = lib.mkEnableOption "Enable Pipewire audio.";
  };

  config = lib.mkIf cfg.enable {
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };
    services.printing.enable = true;
  };
}
