{ lib, ... }:
with lib;
{
  # Use path relative to root of the project
  relativeToRoot = path.append ../.;
}
