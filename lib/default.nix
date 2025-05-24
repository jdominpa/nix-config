{ lib, ... }:
{
  # Use path relative to root of the project
  relativeToRoot = lib.path.append ../.;
}
