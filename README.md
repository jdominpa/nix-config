My personal NixOS/macOS configuration using [nix](https://nixos.org),
[flakes](https://nixos.wiki/wiki/Flakes) and
[home-manager](https://nixos.wiki/wiki/Home_Manager).

## Structure

- `flake.nix`: Entrypoint to the configuration.
- `lib`: Personal library containing some helper functions and variables.
- `hosts`: Directory with host specific configurations. Each subdirectory
  corresponds to a specific host and contains both the system and home-manager
  configuration for that host.
  - `alpha`: Desktop PC - 64GB RAM, Intel i9-10850K, RTX 3090 | KDE Plasma
  - `beta`: Macbook for work
- `home`: Home manager configuration modules.
  - `base`: Home manager modules suitable for both Linux and macOS systems.
  - `linux`: Home manager modules specific to Linux systems.
  - `darwin`: Home manager modules specific to macOS systems.
- `modules`: System configuration modules.
  - `base`: Common system modules for both NixOS and macOS
    ([nix-darwin](https://github.com/LnL7/nix-darwin/tree/master)).
  - `nixos`: System modules specific to NixOS.
  - `darwin`: System modules specific to nix-darwin.
- `overlays`: Patches and overrides for some packages.
- `pkgs`: Custom packages accessible via `nix build`.
- `config`: Directory containing dotfiles that are symlinked with home-manager.

## Deployment

```sh
# Prepare the deployment environment
nix-shell -p just bash

# Deploy using `just` & Justfile
just switch <hostname>

# Alternatively, directly deploy a specific configuration
just <hostname>

# To deploy with details use the `debug` flag
just switch <hostname> debug
# or
just <hostname> debug
```

On NixOS the configuration can also be deployed with the usual commands:
```sh
sudo nixos-rebuild switch --flake .#<hostname>
```

## References

Other nix configuration that inspired this one:

- [Misterio77/nix-config](https://github.com/Misterio77/nix-config/tree/main)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config/tree/main)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
