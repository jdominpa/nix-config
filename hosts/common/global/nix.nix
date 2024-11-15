{lib, ...}: {
  nix = {
    settings = {
      auto-optimise-store = lib.mkDefault true;
      experimental-features = "nix-command flakes";
      warn-dirty = false;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d"; # Keep generations from the last 7 days
    };
  };
}
