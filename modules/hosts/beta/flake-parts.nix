{
  inputs,
  lib,
  ...
}:
{
  flake.darwinConfigurations.beta = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    modules = [
      { nixpkgs.hostPlatform = lib.mkDefault "aarch-darwin"; }
      inputs.self.modules.darwin.beta
    ];
  };
}
