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

  outputs =
    {
      nixpkgs,
      nix-darwin,
      pre-commit-hooks,
      self,
      ...
    }@inputs:
    let
      inherit (self) outputs;
      # inherit (nixpkgs) lib;
      lib = nixpkgs.lib.extend (self: super: { jdp = import ./lib { lib = self; }; });
      # myLib = import ./lib { inherit lib; };
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = lib.genAttrs systems;
      pkgsFor = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        }
      );
    in
    {
      overlays = import ./overlays { inherit inputs; };

      # Custom packages.
      packages = forAllSystems (system: import ./pkgs pkgsFor.${system});
      # Formatter for the nix code in the flake
      formatter = forAllSystems (system: pkgsFor.${system}.nixfmt-rfc-style);

      checks = forAllSystems (system: {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt-rfc-style.enable = true; # formatter
          };
        };
      });

      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsFor.${system};
        in
        {
          default = pkgs.mkShell {
            name = "nix-config";
            inherit (self.checks.${system}.pre-commit-check) shellHook;
            buildInputs = self.checks.${system}.pre-commit-check.enabledPackages;
          };
        }
      );

      # NixOS hosts
      nixosConfigurations = {
        # Desktop
        alpha = lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = {
            inherit inputs outputs;
          };
          modules = [ ./hosts/alpha ];
        };
      };

      # Darwin hosts
      darwinConfigurations = {
        # Work laptop
        beta = myLib.darwinSystem {
          inherit inputs myLib;
          system = "aarch64-darwin";
          specialArgs = {
            inherit inputs outputs myLib;
          };
          darwinModules = [
            ./hosts/beta
            ./modules/base
            ./modules/darwin
          ];
          homeManagerModules = [
            ./hosts/beta/home.nix
            ./modules/home
          ];
        };
      };
    };
}
