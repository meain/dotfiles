-- lsp
local lsp_attach = function(client)
  require "lsp-status".on_attach(client)
end
require "lspconfig".pyls.setup {on_attach = lsp_attach}
require "lspconfig".rust_analyzer.setup {on_attach = lsp_attach}
require "lspconfig".html.setup {on_attach = lsp_attach} -- npm install -g vscode-html-languageserver-bin
require "lspconfig".tsserver.setup {on_attach = lsp_attach}
require "lspconfig".vimls.setup {on_attach = lsp_attach} -- npm install -g vim-language-server
require "lspconfig".gopls.setup {on_attach = lsp_attach}
require "lspconfig".bashls.setup {filetypes = {"sh", "zsh"}, on_attach = lsp_attach}
require "lspconfig".cssls.setup {on_attach = lsp_attach} -- npm install -g vscode-css-languageserver-bin
require "lspconfig".dockerls.setup {on_attach = lsp_attach} -- npm install -g dockerfile-language-server-nodejs
require "lspconfig".sumneko_lua.setup {
  on_attach = lsp_attach,
  settings = {
    Lua = {
      diagnostics = {enable = true, globals = {"hs", "vim", "describe", "it", "before_each", "after_each"}}
    }
  },
  cmd = {
    "/Users/meain/.cache/nvim/nvim_lsp/sumneko_lua/lua-language-server/bin/macOS/lua-language-server",
    "-E",
    "/Users/meain/.cache/nvim/nvim_lsp/sumneko_lua/lua-language-server/main.lua"
  }
}

-- lsp status
require "lsp-status".register_progress()
