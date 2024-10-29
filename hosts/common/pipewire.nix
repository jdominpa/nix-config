{
  config,
  lib,
  ...
}: {
  options = {
    pipewire.enable = lib.mkEnableOption "Configure the low-level multimedia framework pipewire.";
  };

  config = lib.mkIf config.pipewire.enable {
    security.rtkit.enable = true;
    hardware.pulseaudio.enable = false;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };
}
