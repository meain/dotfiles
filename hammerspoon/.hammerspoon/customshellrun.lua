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
    alert(result)
    hs.pasteboard.setContents(result)
end

return module
