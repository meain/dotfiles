local alert    = require("hs.alert")

local module = {}

module.run = function(script)
    local handle = io.popen(script)
    local result = handle:read("*a")
    handle:close()
    local last_char = string.sub(result, -1)
    if last_char == '\n' then
        result = result:sub(1, -2)
    end
    return result
end

return module
