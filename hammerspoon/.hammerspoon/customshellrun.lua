local utils = require("utils")

local module = {}

module.run = function(script)
    -- no need to specify full path in script, but just too slow
    local result = hs.execute(script, '/bin/zsh')

    -- local result = hs.execute(script)
    return utils.trim(result)
end

return module
