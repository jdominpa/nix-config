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
  environment = {
    systemPackages = with pkgs; [
      curl
      neofetch
      findutils
      emacs
      gawk
      git
      gnugrep
      gnused
      gnutar
      stable.iosevka-comfy.comfy
      just
      neovim
      nushell
      ripgrep
      tree
      wget
      which
      zip
    ];
    variables.EDITOR = "emacsclient -r";
  };
}
