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

return utils
