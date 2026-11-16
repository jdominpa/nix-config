# NixOS / nix-darwin Modules

This directory contains the flake-parts modules of the configuration, organized
by functionality and platform.

## Module Categories

### 1. **Features** (`features/`)

Opt-in features, each in its own file or directory:

- Wrapped programs (Emacs, Git, Kitty, Niri, Noctalia, Zsh)
- Applications and tools (Brave, Bitwarden, Discord, Steam, Syncthing, LaTeX)
- Services tied to a program (restic backups, Kanata)
- The user definition used by the other modules

### 2. **Flake-parts** (`flake-parts/`)

Flake-level wiring:

- Supported systems and the flake-parts modules in use
- Formatting and pre-commit hooks

### 3. **Hosts** (`hosts/`)

One directory per machine:

- The system definition (`nixosConfigurations` or `darwinConfigurations`)
- The list of modules the host imports
- Host-specific settings, disk layout and hardware configuration

### 4. **System** (`system/`)

System-wide configuration:

- **Core**: settings shared by every host (Nix, locale, fonts, hardening, boot,
  basic services)
- **Desktop**: graphical environment, themes, login manager, application
  sandboxing and macOS system defaults

## Usage

Every module in `modules/` is automatically imported at the flake level by
[import-tree](https://github.com/denful/import-tree). Hosts decide which modules
they use by importing them in their configuration through the flake-level
options `flake.modules.nixos` and `flake.modules.darwin`.
