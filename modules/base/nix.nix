{
  myLib,
  outputs,
  ...
}: {
  nixpkgs = {
    config.allowUnfree = true;
    overlays = builtins.attrValues outputs.overlays;
  };
  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    trusted-users = [myLib.vars.username];
    substituters = [
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
