{
  inputs,
  ...
}:
let
  hostName = "beta";
in
{
  flake.darwinConfigurations.${hostName} = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    modules = [
      { nixpkgs.hostPlatform = inputs.nixpkgs.lib.mkDefault "aarch64-darwin"; }
      inputs.self.modules.darwin.${hostName}
    ];
  };
}
