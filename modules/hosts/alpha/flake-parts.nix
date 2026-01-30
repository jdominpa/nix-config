{
  inputs,
  ...
}:
let
  hostName = "alpha";
in
{
  flake.nixosConfigurations.${hostName} = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [ inputs.self.modules.nixos.${hostName} ];
  };
}
