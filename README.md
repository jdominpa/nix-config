Modular NixOS/macOS configuration using [nix](https://nixos.org),
[flakes](https://nixos.wiki/wiki/Flakes) and
[home-manager](https://nixos.wiki/wiki/Home_Manager).

## Structure

- `flake.nix`: Entrypoint to the configuration.
- `lib`: Personal library containing helper functions.
- `hosts`: Directory with host specific configurations. Each subdirectory
  corresponds to a specific host and contains both the system and home-manager
  configuration for that host. This is done by enabling/disabling the modules
  that can be found in `modules`.
  - `alpha`: Desktop PC - 64GB RAM, Intel i9-10850K, RTX 3090 | KDE Plasma
  - `beta`: Macbook for work
- `modules`: Configuration modules. Each subdirectory contains a `default.nix`
  that imports all modules, i.e., `*.nix` files (including those in nested
  subdirectories). Each module declares an enable option to toggle
  it. Additionally, it may declare extra options to further customize that
  module.
  - `base`: Common system modules for both NixOS and Darwin (macOS). Notably, it
    contains the `user` submodule used to declare the user's information and
    whether to enable home-manager or not.
  - `nixos`: System modules specific to NixOS.
  - `darwin`: System modules specific to nix-darwin.
  - `home`: Home-manager modules usable by both NixOS and Darwin.
- `overlays`: Patches and overrides for some packages.
- `pkgs`: Custom packages accessible via `nix build`.
- `config`: Directory containing dotfiles that are symlinked with home-manager.

## Deployment

```sh
# Prepare the deployment environment
nix-shell -p just

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

- [Misterio77/nix-config](https://github.com/Misterio77/nix-config)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [thursdaddy/nixos-config](https://github.com/thursdaddy/nixos-config)
