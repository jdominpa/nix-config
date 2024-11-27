{
  config,
  lib,
  myLib,
  ...
}:
with lib;
let
  cfg = config.jdp.home.editors.nvim;
  user = config.jdp.base.user;
in
{
  options.jdp.home = {
    editors.nvim.enable = mkEnableOption "Enable personal neovim configuration.";
  };

  config = mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.file = {
        ".config/nvim/after".source = myLib.relativeToRoot "config/nvim/after";
        ".config/nvim/snippets".source = myLib.relativeToRoot "config/nvim/snippets";
        ".config/nvim/lua".source = myLib.relativeToRoot "config/nvim/lua";
        ".config/nvim/init.lua".source = myLib.relativeToRoot "config/nvim/init.lua";
      };
    };
  };
}
