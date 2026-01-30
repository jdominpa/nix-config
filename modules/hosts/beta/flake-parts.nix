{
  inputs,
  ...
}:
{
  flake.darwinConfigurations.beta = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    modules = [
      { nixpkgs.hostPlatform = inputs.nixpkgs.lib.mkDefault "aarch-darwin"; }
      inputs.self.modules.darwin.beta
    ];
  };
}
