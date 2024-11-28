{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.jdp.base.user;
in
{
  options.jdp.base = {
    user = {
      enable = mkEnableOption "Enable user.";
      name = mkOption {
        type = types.str;
        default = "jdominpa";
        description = "Username.";
        example = "foo";
      };
      fullName = mkOption {
        type = types.str;
        default = "";
        description = "User's full name.";
        example = "Foo Bar";
      };
      email = mkOption {
        type = types.str;
        default = "";
        description = "User's email address.";
        example = "foo@bar.com";
      };
      shell = mkOption {
        type = types.nullOr types.shellPackage;
        default = pkgs.bash;
        description = "User's shell.";
        example = pkgs.zsh;
      };
      homeDirectory = mkOption {
        type = types.str;
        default = "/home/${cfg.name}";
        description = "User's home directory path.";
        example = "/home/foo";
      };
      home-manager.enable = mkEnableOption "Enable home-manager for this user.";
    };
  };

  config = mkIf cfg.enable {
    users.users.${cfg.name} = {
      home = cfg.homeDirectory;
      shell = cfg.shell;
    };
  };
}
