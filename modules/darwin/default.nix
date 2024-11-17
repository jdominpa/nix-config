{myLib, ...}: {
  imports =
    (myLib.scanPaths ./.)
    ++ [
      ../base.nix
    ];
}
