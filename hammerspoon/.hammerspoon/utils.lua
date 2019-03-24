local utils = {}

function utils.reverse (arr)
	local i, j = 1, #arr
	while i < j do
		arr[i], arr[j] = arr[j], arr[i]
		i = i + 1
		j = j - 1
	end
    return arr
end


function utils.trim(s)
    return s:gsub("^%s+", ""):gsub("%s+$", "")
end

function utils.slice(tbl, first, last, step)
  local sliced = {}

  for i = first or 1, last or #tbl, step or 1 do
    sliced[#sliced+1] = tbl[i]
  end

  return sliced
end

return utils
