{
  description = "My NixOS/macOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";

    # nix-darwin
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Home manager
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    darwin,
    home-manager,
    systems,
    ...
  } @ inputs: let
    inherit (self) outputs;
    lib = nixpkgs.lib // home-manager.lib;
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
    # Formatter for your nix files, available through 'nix fmt'
    formatter = forEachSystem (pkgs: pkgs.alejandra);

    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#hostname'
    nixosConfigurations = {
      # Desktop
      alpha = lib.nixosSystem {
        modules = [./hosts/alpha];
        specialArgs = {inherit inputs outputs;};
      };
    };

    # Standalone home-manager configuration entrypoint
    # Available through 'home-manager --flake .#username@hostname'
    homeConfigurations = {
      # Desktop
      "jdominpa@alpha" = lib.homeManagerConfiguration {
        modules = [./home-manager/alpha ./home-manager/nixpkgs.nix];
        pkgs = pkgsFor.x86_64-linux;
        extraSpecialArgs = {inherit inputs outputs;};
      };
    };
  };
}
