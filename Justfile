# Use nushell for shell commands
# To usage this justfile, you need to enter a shell with just & nushell installed:
# 
#   nix shell nixpkgs#just nixpkgs#nushell
set shell := ["nu", "-c"]
utils_nu := absolute_path("utils.nu")

############################################################################
#
#  Common commands(suitable for all machines)
#
############################################################################

# List all the just commands
default:
  @just --list

# Update all the flake inputs
[group('nix')]
up:
  nix flake update

# Update specific input
# Usage: just upp nixpkgs
[group('nix')]
upp input:
  nix flake update {{input}}

# List all generations of the system profile
[group('nix')]
history:
  nix profile history --profile /nix/var/nix/profiles/system

# Open a nix shell with the flake
[group('nix')]
repl:
  nix repl -f flake:nixpkgs

# Remove all generations older than 7 days
[group('nix')]
clean:
  # On darwin you may need to switch to root user to run this command
  sudo nix profile wipe-history --profile /nix/var/nix/profiles/system  --older-than 7d

# Garbage collect all unused nix store entries
[group('nix')]
gc:
  # garbage collect all unused nix store entries (system-wide)
  sudo nix-collect-garbage --delete-older-than 7d
  # garbage collect all unused nix store entries (for the user - home-manager)
  # https://github.com/NixOS/nix/issues/8508
  nix-collect-garbage --delete-older-than 7d

# Enter a shell session which has all the necessary tools for this flake
[group('nix')]
shell:
  nix shell nixpkgs#git nixpkgs#neovim

# Format the nix files in this repo
[group('nix')]
fmt:
  nix fmt

# Nix Store can contains corrupted entries if the nix store object has been modified unexpectedly.
# This command will verify all the store entries,
# and we need to fix the corrupted entries manually via `sudo nix store delete <store-path-1> <store-path-2> ...`
# Verify all the store entries
[group('nix')]
verify-store:
  nix store verify --all

# Repair Nix Store Objects
[group('nix')]
repair-store *paths:
  nix store repair {{paths}}

############################################################################
#
#  NixOS Desktop related commands
#
############################################################################

# Rebuild and switch to the specified NixOS configuration
# Usage: just switch alpha
[linux]
[group('NixOS')]
switch name mode="default":
  #!/usr/bin/env nu
  use {{utils_nu}} *;
  nixos-switch {{name}} {{mode}};

# Deploy alpha's configuration (Desktop PC)
[linux]
[group('NixOS')]
alpha mode="default": (switch "alpha" mode)

############################################################################
#
#  Darwin related commands, harmonica is my macbook pro's hostname
#
############################################################################

[macos]
[group('Darwin')]
darwin-rollback:
  #!/usr/bin/env nu
  use {{utils_nu}} *;
  darwin-rollback

# Rebuild and switch to the specified Darwin configuration
# Usage: just switch beta
[macos]
[group('Darwin')]
switch name mode="default":
  #!/usr/bin/env nu
  use {{utils_nu}} *;
  darwin-build {{name}} {{mode}};
  darwin-switch {{name}} {{mode}};

# Reset launchpad to force it to reindex Applications
[macos]
[group('Darwin')]
reset-launchpad:
  defaults write com.apple.dock ResetLaunchPad -bool true
  killall Dock
