# NixOS / nix-darwin Modules

This directory contains the flake-parts modules of the configuration, organized
by functionality and platform.

## Current Structure

```
modules/
├── README.md
├── features/                    # Per-program configuration
│   ├── bitwarden.nix           # Bitwarden desktop and its SSH agent socket
│   ├── brave.nix               # Brave browser (sandboxed on NixOS)
│   ├── cli-tools.nix           # Command line tools
│   ├── discord.nix             # Vesktop (sandboxed on NixOS)
│   ├── emacs/
│   │   ├── default.nix         # Emacs wrapper and its packages
│   │   └── config/             # Emacs Lisp sources (init.el, modules/, templates/)
│   ├── git.nix                 # Git wrapper and settings
│   ├── kanata.nix              # Keyboard remapping (macOS)
│   ├── kitty.nix               # Kitty terminal wrapper
│   ├── latex.nix               # TeX Live
│   ├── niri/                   # Niri compositor wrapper
│   │   ├── binds.nix           # Key bindings
│   │   ├── default.nix         # Niri module and portals
│   │   ├── input.nix           # Input devices
│   │   ├── layout.nix          # Layout and appearance
│   │   └── rules.nix           # Window rules
│   ├── noctalia.nix            # Noctalia shell wrapper and settings
│   ├── restic.nix              # Encrypted off-site backups
│   ├── steam.nix               # Steam and gaming support
│   ├── syncthing.nix           # File synchronization
│   ├── user.nix                # User definition and account
│   └── zsh.nix                 # Zsh wrapper and settings
├── flake-parts/                 # Flake-level wiring
│   ├── flake-parts.nix         # Systems, flake-parts modules and wrapper installs
│   └── fmt.nix                 # treefmt and pre-commit hooks
├── hosts/                       # One directory per machine
│   ├── alpha/                  # NixOS desktop
│   │   ├── default.nix         # Host definition and module list
│   │   ├── disko.nix           # Disk layout
│   │   └── hardware.nix        # Hardware configuration
│   └── beta/
│       └── default.nix         # nix-darwin host definition and module list
└── system/                      # System-wide configuration
    ├── core/                    # Shared by every host
    │   ├── default.nix         # Aggregates the core modules
    │   ├── fonts.nix           # System fonts
    │   ├── hardening.nix       # Kernel and network hardening
    │   ├── homebrew.nix        # Homebrew (macOS)
    │   ├── locale.nix          # Locale and time zone
    │   ├── nix.nix             # Nix settings and nixpkgs configuration
    │   ├── power-profiles.nix  # Power management
    │   ├── secure-boot.nix     # Secure Boot with lanzaboote
    │   └── services/           # Small system services
    │       ├── bluetooth.nix
    │       ├── pipewire.nix
    │       ├── printing.nix
    │       └── ssh.nix
    └── desktop/                 # Desktop environment
        ├── darwin-settings/    # macOS system defaults
        │   ├── default.nix
        │   ├── dock.nix
        │   ├── keyboard.nix
        │   └── trackpad.nix
        ├── default.nix         # Aggregates the desktop modules
        ├── desktop-tools.nix   # File manager, image viewer, sandboxed mpv
        ├── gtk.nix             # GTK, icon and cursor theme
        ├── login-manager.nix   # Login manager
        └── nixpak.nix          # Application sandboxing policy
```

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

Every module in the `modules/` is automatically imported at the flake level by
[import-tree](https://github.com/denful/import-tree). Hosts decide which modules
they use by importing them in their configuration through the flake-level
options `flake.modules.nixos` and `flake.modules.darwin`.
