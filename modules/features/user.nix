{
  config,
  lib,
  ...
}:
let
  inherit (config.user) username homeDirectory;
  sharedSettings =
    { config, ... }:
    {
      nix.settings.trusted-users = [ username ];
      users.users.${username}.shell = config.wrappers.zsh.wrapper;
    };
in
{
  options.user = lib.mkOption {
    description = ''
      Attribute set containing the user's information used in other modules.

      Must be declared at the flake level since it is also used in wrapper
      packages built in perSystem where no nixos, darwin or home-manager
      `config` exists.
    '';
    type = lib.types.submodule (
      { config, ... }:
      {
        options = {
          username = lib.mkOption {
            type = lib.types.str;
            example = "Foo";
            description = "Login name.";
          };
          fullName = lib.mkOption {
            type = lib.types.str;
            example = "Foo Bar";
            description = "Full name.";
          };
          email = lib.mkOption {
            type = lib.types.str;
            example = "foo@bar.com";
            description = "Email address.";
          };
          signingKey = lib.mkOption {
            type = lib.types.str;
            description = "Public SSH key to sign git commits with.";
          };
          homeDirectory = {
            linux = lib.mkOption {
              type = lib.types.str;
              default = "/home/${config.username}";
              description = "Home directory on NixOS.";
            };
            darwin = lib.mkOption {
              type = lib.types.str;
              default = "/Users/${config.username}";
              description = "Home directory on darwin.";
            };
          };
        };
      }
    );
  };

  config = {
    user = {
      username = "jdominpa";
      fullName = "Joan Domingo Pasarin";
      email = "work@jdompas.com";
      signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGebTck6crA64QvOnpPVBHgB7nzIX18+FU9nANAaE2W4";
    };

    flake.modules.nixos.user = {
      imports = [ sharedSettings ];
      users.users.${username} = {
        isNormalUser = true;
        extraGroups = [
          "networkmanager"
          "wheel"
        ];
        home = homeDirectory.linux;
      };
    };

    flake.modules.darwin.user = {
      imports = [ sharedSettings ];
      users.users.${username} = {
        isHidden = false;
        home = homeDirectory.darwin;
      };
      system.primaryUser = username;
    };
  };
}
