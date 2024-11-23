return {
  {
    "lervag/vimtex",
    ft = { "tex", "bib" },
    config = function()
      vim.g.vimtex_view_method = 'zathura'
      vim.g.vimtex_quickfix_mode = 2
      vim.g.vimtex_quickfix_open_on_warning = 0
    end,
  },
}
