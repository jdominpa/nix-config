{
  config,
  lib,
  ...
}:
let
  cfg = config.jdp.home.editors.nvim;
  inherit (config.jdp.base) user;
in
{
  options.jdp.home = {
    editors.nvim.enable = lib.mkEnableOption "Enable personal neovim configuration.";
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${user.name} = {
      home.file = {
        ".config/nvim/after".source = lib.jdp.relativeToRoot "config/nvim/after";
        ".config/nvim/snippets".source = lib.jdp.relativeToRoot "config/nvim/snippets";
        ".config/nvim/lua".source = lib.jdp.relativeToRoot "config/nvim/lua";
        ".config/nvim/init.lua".source = lib.jdp.relativeToRoot "config/nvim/init.lua";
      };
    };
  };
}
