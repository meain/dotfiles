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
