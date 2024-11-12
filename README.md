My personal NixOS/macOS configuration using [nix](https://nixos.org),
[flakes](https://nixos.wiki/wiki/Flakes) and
[home-manager](https://nixos.wiki/wiki/Home_Manager). The configuration is
heavily inspired by Misterio77's [starter config
repo](https://github.com/Misterio77/nix-starter-config) and his personal
[nix-config repo](https://github.com/Misterio77/nix-config/tree/main).

## Structure

- `flake.nix`: Entrypoint to the configuration. Additionally, it also exposes a
  devshell for bootstraping the configuration.
- `hosts`: System wide configurations accessible via `nixos-rebuild --flake` or
  `darwin-rebuild --flake`.
  - `common`: Directory containing shared configurations that are used by the
    system specific ones.
    - `global`: Configurations applied to every system.
    - `optional`: Opt-in configurations that can be enabled via boolean options.
    - `users`: Declaration and configuration of users.
  - `alpha`: Desktop PC - 64GB RAM, Intel i9-10850K, RTX 3090 | KDE Plasma
- `home-manager`: Home-manager configuration accessible via `home-manager --flake`.
  - This directory follows the same structure as `hosts`, that is, it has a
    `common` directory for shared configurations (with `global` and `optional`
    subdirectories) and a directory for each host present in `hosts`.
- `modules`: Modules (with options) that haven't been upstreamed yet.
- `overlays`: Patches and overrides for some packages (accessible via `nix
  build`).
- `pkgs`: Custom packages accessible via `nix build`.
- `config`: Directory containing dotfiles that are symlinked with home-manager.

## How to bootstrap

All that is needed is nix (any version). Run:
```
$ nix-shell
```
If you are using nix 2.4+, git, and have `flakes` and `nix-command` enabled, you can also use the command
```
$ nix develop
```

`nixos-rebuild switch --flake .` to build system configurations (in NixOS)

`home-manager switch --flake .` to build user configurations

`nix build` (or `shell` or `run`) to build and use packages
