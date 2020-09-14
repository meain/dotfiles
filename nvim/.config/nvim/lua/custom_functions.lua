local mod = {}

mod.get_current_diagnostics = function()
  local diag_items = vim.lsp.util.get_line_diagnostics()
  local diagnostics_outputs = {}
  for _, v in pairs(diag_items) do
    table.insert(diagnostics_outputs, v.message)
  end
  return table.concat(diagnostics_outputs, ' :: ')
end

-- print(vim.inspect(mod))

return mod
