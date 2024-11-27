{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.jdp.nixos.system.pipewire;
in
{
  options.jdp.nixos = {
    system.pipewire.enable = mkEnableOption "Enable Pipewire audio.";
  };

  config = mkIf cfg.enable {
    security.rtkit.enable = true;
    hardware.pulseaudio.enable = false;
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
