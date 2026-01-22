############################################################################
#
#  Common commands(suitable for all machines)
#
############################################################################

[private]
default:
	@echo "error: no subcommand specified"
	@echo "Try 'just help' for more information."

# List all subcommands
help:
	@just --list

# Update flake inputs (all of them if none are specified)
[group('nix')]
update *INPUTS:
    nix flake update {{INPUTS}}

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
    nix store gc

# Garbage collect all unused nix store entries
[group('nix')]
gc:
    # garbage collect all unused nix store entries (system-wide)
    sudo nix-collect-garbage --delete-older-than 7d
    # garbage collect all unused nix store entries (for the user - home-manager)
    # https://github.com/NixOS/nix/issues/8508
    nix-collect-garbage --delete-older-than 7d

# Check whether the flake evaluates and run its tests
[group('nix')]
check *FLAGS:
    nix flake check --no-update-lock-file --no-write-lock-file {{FLAGS}}

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

# Rebuild and switch the specified NixOS configuration
[linux]
[group('NixOS')]
switch host *FLAGS:
    sudo nixos-rebuild switch --flake "{{justfile_directory()}}#{{host}}" {{FLAGS}}

############################################################################
#
#  Darwin related commands, harmonica is my macbook pro's hostname
#
############################################################################

# Rollback configuration
[macos]
[group('Darwin')]
rollback:
    darwin-rebuild --rollback

# Rebuild and switch the specified Darwin configuration
[macos]
[group('Darwin')]
switch host *FLAGS:
    #!/usr/bin/env bash
    if ! command -v darwin-rebuild 2>&1 >/dev/null; then
        sudo nix run nix-darwin/master#darwin-rebuild --extra-experimental-features "nix-command flakes" -- switch --flake "{{justfile_directory()}}#{{host}}" {{FLAGS}}
    else
        sudo darwin-rebuild switch --flake "{{justfile_directory()}}#{{host}}" {{FLAGS}}
    fi

# Reset launchpad to force it to reindex Applications
[macos]
[group('Darwin')]
reset-launchpad:
    defaults write com.apple.dock ResetLaunchPad -bool true
    killall Dock
