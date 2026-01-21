{
  config,
  inputs,
  lib,
  ...
}:
let
  cfg = config.jdp.nixos.desktop.noctalia;
in
{
  imports = [
    inputs.noctalia.nixosModules.default
  ];

  options.jdp.nixos = {
    desktop.noctalia.enable = lib.mkEnableOption "Whether to enable the Noctalia shell.";
  };

  config = lib.mkIf cfg.enable {
    services.noctalia-shell.enable = true;
  };
}
