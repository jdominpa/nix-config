{
  description = "My NixOS/Darwin configuration";

  nixConfig = {
    extra-substituters = [
      # Nix community's cache server
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      # Nix community's cache server public key
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    import-tree.url = "github:vic/import-tree";
    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    niri-flake = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);
}
# {
#   # Formatter for the nix code in the flake
#   formatter = forAllSystems (system: pkgsFor.${system}.nixfmt);

#   checks = forAllSystems (system: {
#     pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
#       src = ./.;
#       hooks = {
#         nixfmt.enable = true; # formatter
#       };
#     };
#   });

#   devShells = forAllSystems (system: {
#     default = pkgsFor.${system}.mkShell {
#       name = "nix-config";
#       inherit (self.checks.${system}.pre-commit-check) shellHook;
#       buildInputs = self.checks.${system}.pre-commit-check.enabledPackages;
#     };
#   });

#   # NixOS hosts
#   nixosConfigurations = {
#     # Desktop
#     nixos = lib.nixosSystem {
#       system = "x86_64-linux";
#       specialArgs = {
#         inherit inputs outputs;
#       };
#       modules = [ ./hosts/nixos ];
#     };
#   };

#   # Darwin hosts
#   darwinConfigurations = {
#     # Work laptop
#     macbook = nix-darwin.lib.darwinSystem {
#       system = "aarch64-darwin";
#       specialArgs = {
#         inherit inputs outputs lib;
#       };
#       modules = [ ./hosts/macbook ];
#     };
#   };
# };
