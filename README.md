[Dendritic](https://github.com/mightyiam/dendritic) NixOS/macOS configuration using [nix](https://nixos.org), [flakes](https://nixos.wiki/wiki/Flakes), [nix-darwin](https://github.com/nix-darwin/nix-darwin) and
[home-manager](https://nixos.wiki/wiki/Home_Manager).

## Deployment

### NixOS

```bash
# Prepare the deployment environment
nix-shell -p git just

# Deploy using `just` & Justfile
just switch <hostname>
```

On NixOS the configuration can also be deployed with the usual commands:

```bash
sudo nixos-rebuild switch --flake .#<hostname>
```

### macOS

For first time deployment, install [nix](https://nixos.org/download/#nix-install-macos) and [homebrew](https://brew.sh) manually. Then proceed as
follows:

```bash
# Prepare the deployment environment
nix-shell -p git just

# Deploy using `just` & Justfile
just switch <hostname>
```

## References

Other nix configuration that inspired this one:

- [Misterio77/nix-config](https://github.com/Misterio77/nix-config)
- [ryan4yin/nix-config](https://github.com/ryan4yin/nix-config)
- [hlissner/dotfiles](https://github.com/hlissner/dotfiles)
- [drupol/infra](https://github.com/drupol/infra)

Also, for more information on the dendritic pattern see:

- [Dendritic Design](https://github.com/Doc-Steve/dendritic-design-with-flake-parts)
- [Dendrix](https://dendrix.oeiuwq.com/index.html)
