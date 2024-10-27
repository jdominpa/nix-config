{
  config,
  lib,
  ...
}: {
  options = {
    pipewire.enable = lib.mkOption {
      description = "Enable pipewire service and configure it.";
      type = lib.types.bool;
      default = false;
    };
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
