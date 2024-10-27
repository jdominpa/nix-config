# How to bootstrap

All that is needed is nix (any version). Run:
```
nix-shell
```
If you are using nix 2.4+, git, and have `flakes` and `nix-command` enabled, you can also use the command
```
nix develop
```

`nixos-rebuild switch --flake .` to build system configurations (in NixOS)

`home-manager switch --flake .` to build user configurations

`nix build` (or `shell` or `run`) to build and use packages