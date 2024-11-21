{
  # Auto upgrade the nix-daemon service
  services.nix-daemon.enable = true;

  nix = {
    # Disable auto-optimise-store because of this issue:
    #   https://github.com/NixOS/nix/issues/7273
    # "error: cannot link '/nix/store/.tmp-link-xxxxx-xxxxx' to '/nix/store/.links/xxxx': File exists"
    settings.auto-optimise-store = false;

    # TODO: try automatic garbage collection on macOS
    gc.automatic = false;
  };
}
