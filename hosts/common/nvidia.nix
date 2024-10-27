{
  config,
  lib,
  ...
}: {
  options = {
    nvidia.enable = lib.mkOption {
      description = "Enable nvidia drivers and configure them.";
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf config.nvidia.enable {
    hardware.opengl = {
      enable = true;
    };
    services.xserver.videoDrivers = ["nvidia"];
    hardware.nvidia = {
      modesetting.enable = true;
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.latest;
    };
  };
}
