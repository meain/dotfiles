local mod = {}

local pasteboard = require("hs.pasteboard")

local customshellrun = require('customshellrun') 
local utils = require("utils")

-- TODO: move this to 'datafiles' dir and read from there
doables = {'add-to-gourcer', 'open', 'openorsearch', 'gource', 'tempg'}

function mod.clipdo()
    function formatChoices(choices)
        formattedChoices = hs.fnutils.imap(choices, function(item)
            return { ["text"] = utils.trim(item) }
        end)
        return formattedChoices
    end

    local link = utils.trim(pasteboard.getContents())
    local chooser = hs.chooser.new(function(chosen)
        hs.alert("🧧 " .. chosen.text .. " '" .. link .. "'")
        result = customshellrun.run(chosen.text .. " '" .. link .. "'", true)
        if (result == nil or result == '') then
            hs.alert('-- no output --')
        else
            hs.alert(result)
        end
    end)
    chooser:placeholderText(link)
    chooser:choices(formatChoices(doables))
    chooser:show()
end


function mod.registerDefaultBindings(mods, key)
    mods = mods or {"alt"}
    key = key or "L"
    hs.hotkey.bind(mods, key, mod.clipdo)
end

return mod
