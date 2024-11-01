{
  inputs,
  outputs,
  ...
}: {
  imports = [
    ./locale.nix
    ./nix.nix
    ./openssh.nix
    ./zsh.nix
  ];

  home-manager = {
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs outputs;};
  };

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config.allowUnfree = true;
  };
}
