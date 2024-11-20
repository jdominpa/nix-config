{
  outputs,
  pkgs,
  myLib,
  ...
}: {
  nixpkgs.overlays = builtins.attrValues outputs.overlays;
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
    trusted-users = [myLib.vars.username];
    substituters = [
      "https://nix-community.cachix.org"
    ];
  };
}
