local utils = {}

function utils.reverse(arr)
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
        sliced[#sliced + 1] = tbl[i]
    end

    return sliced
end

function utils.count(s, c)
    local _, n = s:gsub(c, "")
    return n
end

function utils.split(inputstr, sep)
    if sep == nil then
        sep = "%s"
    end
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, str)
    end
    return t
end

function utils.tablelength(T)
    local count = 0
    for _ in pairs(T) do
        count = count + 1
    end
    return count
end

function utils.linecount(str)
    lines = {}
    local count = 0
    for s in str:gmatch("[^\r\n]+") do
        table.insert(lines, s)
        count = count + 1
    end
    return count
end

function utils.file_exists(name)
    local f = io.open(name, "r")
    if f ~= nil then
        io.close(f)
        return true
    else
        return false
    end
end

function utils.isin(list, item)
    for _,li in ipairs(list) do
        if li == item then
            return true
        end
    end
    return false
end

function utils.dropEmailFooter(content)
    newContents = ""
    for _, line in ipairs(utils.split(content, '\n')) do
        if line == "-- " then
            break
        else
            newContents = newContents .. line .. "\n"
        end
    end
    return newContents
end

return utils
