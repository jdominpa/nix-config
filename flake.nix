{
  description = "My NixOS/macOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";
    systems.url = "github:nix-systems/default";

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
    systems,
    nix-darwin,
    home-manager,
    plasma-manager,
    ...
  } @ inputs: let
    inherit (self) outputs;
    myVars = {
      username = "jdominpa";
    };
    lib = nixpkgs.lib // nix-darwin.lib // home-manager.lib;
    pkgsFor = lib.genAttrs (import systems) (
      system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        }
    );
    forEachSystem = f: lib.genAttrs (import systems) (system: f pkgsFor.${system});
  in {
    inherit lib;
    nixosModules = import ./modules/nixos;
    homeManagerModules = import ./modules/home-manager;

    overlays = import ./overlays {inherit inputs;};

    # Custom packages.  Accessible through 'nix build', 'nix shell', etc
    packages = forEachSystem (pkgs: import ./pkgs {inherit pkgs;});
    # Expose a devshell for bootstraping the configuration
    devShells = forEachSystem (pkgs: import ./shell.nix {inherit pkgs;});
    # Formatter for your nix files, available through 'nix fmt'
    formatter = forEachSystem (pkgs: pkgs.alejandra);

    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#hostname'
    nixosConfigurations = {
      # Desktop
      alpha = lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./hosts/alpha
          home-manager.nixosModules.home-manager {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = {inherit inputs outputs;};
              users."${myVars.username}" = import ./home/alpha;
            };
          }
        ];
      };
    };
  };
}
