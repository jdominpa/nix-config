{
  outputs,
  myLib,
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
  };
}
