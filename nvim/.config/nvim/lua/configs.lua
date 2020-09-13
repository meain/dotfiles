-- nvim-treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = "all",
  highlight = { enable = true },
  refactor = {
      highlight_definitions = { enable = true },
      highlight_current_scope = { enable = false },
    },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
  },
  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false -- Whether the query persists across vim sessions
  }
}

-- lsp
require'nvim_lsp'.pyls.setup{}
-- require'nvim_lsp'.pyls_ms.setup{}
-- require'nvim_lsp'.jedi_language_server.setup{}
require'nvim_lsp'.rust_analyzer.setup{}
require'nvim_lsp'.html.setup{}  -- npm install -g vscode-html-languageserver-bin
require'nvim_lsp'.tsserver.setup{}
require'nvim_lsp'.vimls.setup{}  -- npm install -g vim-language-server
require'nvim_lsp'.gopls.setup{}
require'nvim_lsp'.bashls.setup{ filetypes = {'sh', 'zsh'}; }
require'nvim_lsp'.cssls.setup{}  -- npm install -g vscode-css-languageserver-bin
require'nvim_lsp'.dockerls.setup{}  -- npm install -g dockerfile-language-server-nodejs
require'nvim_lsp'.sumneko_lua.setup{
    cmd = { "/Users/meain/.cache/nvim/nvim_lsp/sumneko_lua/lua-language-server/bin/macOS/lua-language-server",
        "-E", "/Users/meain/.cache/nvim/nvim_lsp/sumneko_lua/lua-language-server/main.lua" };
}
