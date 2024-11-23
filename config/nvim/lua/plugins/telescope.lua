return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-ui-select.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    keys = {
      { "<leader>f", "<CMD>Telescope find_files<CR>", desc = "Find files" },
      { "<leader>b", "<CMD>Telescope buffers<CR>", desc = "Switch buffer" },
      { "<leader>s", "<CMD>Telescope live_grep<CR>", desc = "Live grep in cwd" },
      { "<leader>h", "<CMD>Telescope help_tags<CR>", desc = ":h" },
      { "<leader>?", "<CMD>Telescope builtin<CR>", desc = "Telescope builtin commands" },
      { "<leader>gs", "<CMD>Telescope git_status<CR>", desc = "Git status" },
      { "<leader>gc", "<CMD>Telescope git_commits<CR>", desc = "Git commits" },
    },
    config = function()
      local telescope = require("telescope")
      telescope.setup({
        defaults = {
          prompt_prefix = "> ",
          selection_caret = "> ",
          winblend = 0,
          layout_strategy = "bottom_pane",
          layout_config = {
            prompt_position = "bottom",
            height = 0.5,
          },
          scroll_strategy = "cycle",
          color_devicons = true,
        },
        extensions = {
          ["ui-select"] = {
            layout_config = { height = 0.25 },
          },
        }
      })

      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")
    end,
  },
}
