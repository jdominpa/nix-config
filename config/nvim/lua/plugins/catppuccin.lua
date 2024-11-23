return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    config = function()
      require("catppuccin").setup({
        flavour = "mocha",
        show_end_of_buffer = true,
        integrations = {
          mason = true,
        },
      })
      vim.cmd.colorscheme("catppuccin")
    end,
  }
}
