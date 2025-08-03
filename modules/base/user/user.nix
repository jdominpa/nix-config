{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.jdp.base.user;
in
{
  options.jdp.base = {
    user = {
      enable = lib.mkEnableOption "Enable user.";
      name = lib.mkOption {
        type = lib.types.str;
        default = "jdominpa";
        description = "Username.";
        example = "foo";
      };
      fullName = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "User's full name.";
        example = "Foo Bar";
      };
      email = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "User's email address.";
        example = "foo@bar.com";
      };
      shell = lib.mkOption {
        type = lib.types.nullOr lib.types.shellPackage;
        default = pkgs.bash;
        description = "User's shell.";
        example = pkgs.zsh;
      };
      homeDirectory = lib.mkOption {
        type = lib.types.str;
        default = "/home/${cfg.name}";
        description = "User's home directory path.";
        example = "/home/foo";
      };
      home-manager.enable = lib.mkEnableOption "Enable home-manager for this user.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.name} = {
      home = cfg.homeDirectory;
      inherit (cfg) shell;
    };
  };
}
