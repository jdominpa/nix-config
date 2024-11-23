{myLib, ...}: {
  home.file = {
    ".config/nvim/after".source = myLib.relativeToRoot "config/nvim/after";
    ".config/nvim/snippets".source = myLib.relativeToRoot "config/nvim/snippets";
    ".config/nvim/lua".source = myLib.relativeToRoot "config/nvim/lua";
    ".config/nvim/init.lua".source = myLib.relativeToRoot "config/nvim/init.lua";
  };
}
