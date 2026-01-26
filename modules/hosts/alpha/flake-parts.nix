{
  inputs,
  ...
}:
{
  flake.nixosConfigurations.alpha = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [ inputs.self.modules.nixos.alpha ];
  };
}
