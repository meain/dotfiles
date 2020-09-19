local M = {}

local function split(result, sep)
  local sep, fields = sep or ":", {}
  local pattern = result.format("([^%s]+)", sep)
  result:gsub(
    pattern,
    function(c)
      fields[#fields + 1] = c
    end
  )
  return fields
end

function M.getCompletionItems(prefix)
  local complete_items = {}
  -- define total completion items
  local notmuch_handle = io.popen("notmuch address " .. prefix)
  local notmuch_result = notmuch_handle:read("*a")
  notmuch_handle:close()

  local abook_handle = io.popen("abook --mutt-query " .. prefix)
  local abook_result = abook_handle:read("*a")
  print("abook_result:", abook_result)
  abook_handle:close()

  -- find matches items and put them into complete_items
  for _, email in ipairs(split(abook_result, "\n")) do
    -- no need for extra string match check here
    table.insert(
      complete_items,
      {
        word = email,
        kind = "abook",
        score = 1,
        icase = 1,
        dup = 1,
        empty = 1
      }
    )
  end
  for _, email in ipairs(split(notmuch_result, "\n")) do
    if string.match(email, prefix) then
      table.insert(
        complete_items,
        {
          word = email,
          kind = "notmuch",
          score = 1,
          icase = 1,
          dup = 1,
          empty = 1
        }
      )
    end
  end
  return complete_items
end

M.complete_item = {
  item = M.getCompletionItems
}

return M
