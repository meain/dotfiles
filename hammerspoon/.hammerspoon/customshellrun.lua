local utils = require("utils")

local module = {}

module.run = function(script, use_zsh)
    use_zsh = use_zsh or false
    local result
    if (use_zsh == true) then
        -- no need to specify full path in script, but just too slow
        result = hs.execute(script, "/bin/zsh")
    else
        -- Do `echo $PATH > ~/.local/share/latestpath` once in a while
        local path = io.open("/Users/meain/.local/share/latestpath"):read()
        result = hs.execute("PATH='"..path.."' " .. script)
    end
    return utils.trim(result)
end

return module
