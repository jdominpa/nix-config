{ myLib, ... }:
{
  imports = myLib.scanPaths ./.;
}
