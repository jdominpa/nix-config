{
  config,
  lib,
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
        ".config/nvim/after".source = jdp.relativeToRoot "config/nvim/after";
        ".config/nvim/snippets".source = jdp.relativeToRoot "config/nvim/snippets";
        ".config/nvim/lua".source = jdp.relativeToRoot "config/nvim/lua";
        ".config/nvim/init.lua".source = jdp.relativeToRoot "config/nvim/init.lua";
      };
    };
  };
}
