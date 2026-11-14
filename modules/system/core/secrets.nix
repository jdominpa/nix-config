{
  config,
  inputs,
  lib,
  ...
}:
let
  inherit (config.user) username homeDirectory;
  # Each host decrypts its own file of the private `secrets` input with the one
  # personal age key, kept where the `sops` CLI looks for it by default on each
  # platform, so editing works without any environment variable. See
  # docs/SECRETS.md for the key, the layout of the secrets repository and the
  # bootstrap.
  sharedSettings =
    keyFile:
    { config, ... }:
    {
      options.secretPresets = lib.mkOption {
        type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
        readOnly = true;
        default = {
          # Read only by root: system services and activation
          rootOnly = {
            owner = "root";
            mode = "0400";
          };
          # Read by the user: services that run as the user, and the user
          userReadable = {
            owner = username;
            mode = "0400";
          };
        };
        description = "Ownership and mode presets for sops secrets.";
      };

      config = {
        sops = {
          defaultSopsFile = "${inputs.secrets}/${config.networking.hostName}.yaml";
          age = {
            inherit keyFile;
            # Only the key above, not keys derived from the SSH host keys
            sshKeyPaths = [ ];
          };
          gnupg.sshKeyPaths = [ ];
        };
      };
    };
in
{
  flake.modules.nixos.secrets = {
    imports = [
      inputs.sops-nix.nixosModules.sops
      (sharedSettings "${homeDirectory.linux}/.config/sops/age/keys.txt")
    ];
    fileSystems."/home".neededForBoot = true;
  };

  flake.modules.darwin.secrets = {
    imports = [
      inputs.sops-nix.darwinModules.sops
      (sharedSettings "${homeDirectory.darwin}/Library/Application Support/sops/age/keys.txt")
    ];
  };
}
