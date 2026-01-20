{ inputs, ... }:
{
  # Emacs community overlay
  emacs-overlay = inputs.emacs-overlay.overlays.default;

  # Niri flake
  niri-flake = inputs.niri-flake.overlays.niri;

  # Bring our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs final.pkgs;

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });
  };

  # When applied, the stable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.stable'
  stable-packages = final: _prev: {
    stable = inputs.nixpkgs-stable.legacyPackages.${final.system};
  };
}
