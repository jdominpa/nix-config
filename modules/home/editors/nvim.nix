{
  config,
  lib,
  myLib,
  ...
}: let
  cfg = config.jdp.home.editors.nvim;
in {
  options.jdp = {
    home.editors.nvim.enable = lib.mkEnableOption "Enable personal neovim configuration.";
  };

  config = lib.mkIf cfg.enable {
    home.file = {
      ".config/nvim/after".source = myLib.relativeToRoot "config/nvim/after";
      ".config/nvim/snippets".source = myLib.relativeToRoot "config/nvim/snippets";
      ".config/nvim/lua".source = myLib.relativeToRoot "config/nvim/lua";
      ".config/nvim/init.lua".source = myLib.relativeToRoot "config/nvim/init.lua";
    };
  };
}
