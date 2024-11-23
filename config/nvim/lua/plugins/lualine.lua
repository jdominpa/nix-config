return {
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    dependencies = { "kyazdani42/nvim-web-devicons", lazy = true },
    config = function()
      require("lualine").setup({
        options = {
          component_separators = '|',
          section_separators = '',
          always_divide_middle = true,
          icons_enabled = true,
          globalstatus = true,
          theme = "auto",
        },
      })
    end,
  },
}
