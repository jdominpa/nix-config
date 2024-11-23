return {
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    build = ":MasonUpdate",
    config = function()
      require("mason").setup({
        ui = { border = "rounded" },
      })
    end
  },

  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
    },
    config = function()
      -- Keymappings
      local on_attach = function(_, bufnr)
        local map = vim.keymap.set
        local opts = { buffer = bufnr }

        vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"
        map("n", "gD", vim.lsp.buf.declaration, opts)
        map("n", "K", vim.lsp.buf.hover, opts)
        map("i", "<C-s>", vim.lsp.buf.signature_help, opts)
        map("n", "<leader>lr", vim.lsp.buf.rename, opts)
        map("n", "<leader>la", vim.lsp.buf.code_action, opts)
        map("n", "<leader>lf", function()
          vim.lsp.buf.format({ async = true })
        end, opts)
        map("n", "[d", vim.diagnostic.goto_prev, opts)
        map("n", "]d", vim.diagnostic.goto_next, opts)

        local has_telescope, telescope = pcall(require, "telescope.builtin")
        if has_telescope then
          map("n", "gd", telescope.lsp_definitions, opts)
          map("n", "gy", telescope.lsp_type_definitions, opts)
          map("n", "gi", telescope.lsp_implementations, opts)
          map("n", "gr", telescope.lsp_references, opts)
          map("n", "<leader>ls", telescope.lsp_document_symbols, opts)
          map("n", "<leader>lS", telescope.lsp_workspace_symbols, opts)
          map("n", "<leader>ld", telescope.diagnostics, opts)
        end
      end

      -- Inform of cmp extra capabilities
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

      -- Rounded window borders
      require("lspconfig.ui.windows").default_options.border = "rounded"

      local servers = {
        -- C/C++
        clangd = {},

        -- Python
        pyright = {},

        -- Latex
        texlab = {},

        -- Lua
        lua_ls = {
          Lua = {
            runtime = { version = "LuaJIT" },
            workspace = {
              checkThirdParty = false,
              library = { vim.env.VIMRUNTIME },
            },
            -- telemetry = { enable = false },
          },
        },
      }

      local mason_lspconfig = require("mason-lspconfig")

      -- Install lsp servers
      mason_lspconfig.setup({
        ensure_installed = vim.tbl_keys(servers),
      })

      -- Setup lsp server configurations
      mason_lspconfig.setup_handlers({
        function(server_name)
          require("lspconfig")[server_name].setup({
            capabilities = capabilities,
            on_attach = on_attach,
            settings = servers[server_name],
            filetypes = (servers[server_name] or {}).filetypes,
          })
        end,
      })
    end,
  },
}
