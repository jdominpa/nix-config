local map = vim.keymap.set

-- Open netrw
map("n", "<leader>pf", vim.cmd.Ex)

-- Make Y behave like the rest of capital actions
map("n", "Y", "yg$")

-- Center search results when jumping between them
map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")

-- Center cursor when scrolling up or down
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")

-- Don't move the screen when joining lines
map("n", "J", "mzJ`z")

-- Move lines
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")

-- Replace highlighted text without losing yanked text
map("x", "<leader>p", "\"_dP")

-- Delete into black hole
map({"n", "v"}, "<leader>d", "\"_d")

-- Yank to "+ register
map({"n", "v"}, "<leader>y", "\"+y")
map("n", "<leader>Y", "\"+Y")

-- Jump to next or previous item in quickfix list
map("n", "]q", "<cmd>cnext<CR>zzzv")
map("n", "[q", "<cmd>cprev<CR>zzzv")
map("n", "]l", "<cmd>lnext<CR>zzzv")
map("n", "[l", "<cmd>lprev<CR>zzzv")
