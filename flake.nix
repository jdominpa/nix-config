{
  description = "My NixOS/macOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";

    # Collection of hardware configurations
    hardware.url = "github:nixos/nixos-hardware";

    # nix-darwin
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Home manager
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Add git hooks to format nix code before commits
    pre-commit-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Plasma manager
    plasma-manager = {
      url = "github:nix-community/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = {
    self,
    nixpkgs,
    pre-commit-hooks,
    ...
  } @ inputs: let
    inherit (self) outputs;
    inherit (nixpkgs) lib;
    myLib = import ./lib {inherit lib;};
    systems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];
    pkgsFor = lib.genAttrs systems (
      system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        }
    );
    forAllSystems = f: lib.genAttrs systems f;
    forEachSystem = f: lib.genAttrs systems (system: f pkgsFor.${system});
  in {
    overlays = import ./overlays {inherit inputs;};

    # Custom packages.  Accessible through 'nix build', 'nix shell', etc
    packages = forEachSystem (pkgs: import ./pkgs {inherit pkgs;});
    # Expose a devshell for bootstraping the configuration
    devShells = forEachSystem (pkgs: import ./shell.nix {inherit pkgs;});
    # Formatter for your nix files, available through 'nix fmt'
    formatter = forEachSystem (pkgs: pkgs.alejandra);

    checks = forAllSystems (
      system: {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = myLib.relativeToRoot ".";
          hooks = {
            alejandra.enable = true; # formatter
            deadnix.enable = true; # detect unused variable bindings in `*.nix`
          };
        };
      }
    );

    # NixOS hosts
    nixosConfigurations = {
      # Desktop
      alpha = myLib.nixosSystem {
        inherit inputs myLib;
        system = "x86_64-linux";
        specialArgs = {inherit inputs outputs myLib;};
        nixosModules = [./hosts/alpha];
        homeManagerModules = [./home/alpha];
      };
    };
  };
}
