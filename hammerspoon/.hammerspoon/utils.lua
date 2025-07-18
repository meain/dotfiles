local pasteboard = require("hs.pasteboard")

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
    for _, li in ipairs(list) do
        if li == item then
            return true
        end
    end
    return false
end

function utils.dropEmailFooter(content)
    newContents = ""
    for _, line in ipairs(utils.split(content, "\n")) do
        if line == "-- " then
            break
        else
            newContents = newContents .. line .. "\n"
        end
    end
    return newContents
end

function utils.printTable(node)
    -- to make output beautiful
    local function tab(amt)
        local str = ""
        for i = 1, amt do
            str = str .. "\t"
        end
        return str
    end

    local cache, stack, output = {}, {}, {}
    local depth = 1
    local output_str = "{\n"

    while true do
        local size = 0
        for k, v in pairs(node) do
            size = size + 1
        end

        local cur_index = 1
        for k, v in pairs(node) do
            if (cache[node] == nil) or (cur_index >= cache[node]) then
                if (string.find(output_str, "}", output_str:len())) then
                    output_str = output_str .. ",\n"
                elseif not (string.find(output_str, "\n", output_str:len())) then
                    output_str = output_str .. "\n"
                end

                -- This is necessary for working with HUGE tables otherwise we run out of memory using concat on huge strings
                table.insert(output, output_str)
                output_str = ""

                local key
                if (type(k) == "number" or type(k) == "boolean") then
                    key = "[" .. tostring(k) .. "]"
                else
                    key = "['" .. tostring(k) .. "']"
                end

                if (type(v) == "number" or type(v) == "boolean") then
                    output_str = output_str .. tab(depth) .. key .. " = " .. tostring(v)
                elseif (type(v) == "table") then
                    output_str = output_str .. tab(depth) .. key .. " = {\n"
                    table.insert(stack, node)
                    table.insert(stack, v)
                    cache[node] = cur_index + 1
                    break
                else
                    output_str = output_str .. tab(depth) .. key .. " = '" .. tostring(v) .. "'"
                end

                if (cur_index == size) then
                    output_str = output_str .. "\n" .. tab(depth - 1) .. "}"
                else
                    output_str = output_str .. ","
                end
            else
                -- close the table
                if (cur_index == size) then
                    output_str = output_str .. "\n" .. tab(depth - 1) .. "}"
                end
            end

            cur_index = cur_index + 1
        end

        if (#stack > 0) then
            node = stack[#stack]
            stack[#stack] = nil
            depth = cache[node] == nil and depth + 1 or depth - 1
        else
            break
        end
    end

    -- This is necessary for working with HUGE tables otherwise we run out of memory using concat on huge strings
    table.insert(output, output_str)
    output_str = table.concat(output)

    print(output_str)
end

function utils.ifClipChanges(success, failure, maxTime)
    local initialClip = pasteboard.getContents()
    local sleepTime = 1
    function clipCheck(initialClip, success, failure, currentTime, maxTime)
        local clip = pasteboard.getContents()
        if currentTime > maxTime then
            failure()
        elseif clip ~= initialClip then
            success()
        else
            hs.timer.doAfter(
                sleepTime,
                function()
                    clipCheck(initialClip, success, failure, currentTime + sleepTime, maxTime)
                end
            )
        end
    end
    clipCheck(initialClip, success, failure, 0, maxTime)
end

function utils.waitTillClipChanges(maxTime)
    local initialClip = pasteboard.getContents()
    local i = maxTime
    while (i > 0) do
        os.execute("sleep " .. tonumber(1))
        if (pasteboard.getContents() ~= initialClip) then
            return true
        end
        i = i - 1
    end
    return false
end

function utils.moveMouseToCurrentWindowScreen()
    local currentApp = hs.window.focusedWindow()
    local screen = currentApp:screen()
    local pt = hs.geometry.rectMidPoint(screen:fullFrame())
    local currentScreen = hs.screen.mainScreen()
    if currentScreen:id() ~= screen:id() then
        hs.mouse.setAbsolutePosition(pt)
    end
end

function utils.moveMouseToScreen(screen)
    local pt = hs.geometry.rectMidPoint(screen:fullFrame())
    hs.mouse.setAbsolutePosition(pt)
end

function utils.dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end


return utils
