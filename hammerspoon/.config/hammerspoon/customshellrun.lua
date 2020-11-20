local utils = require("utils")

local module = {}

module.run = function(script, use_zsh)
    use_zsh = use_zsh or false
    local result
    if (use_zsh == true) then
        -- no need to specify full path in script, but just too slow
        result = hs.execute(script, "/bin/zsh")
    else
        result = hs.execute(script)
    end
    return utils.trim(result)
end

return module
