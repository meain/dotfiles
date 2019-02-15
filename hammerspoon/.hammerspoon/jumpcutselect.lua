-- Selector for jumpcut thingy
local settings = require("hs.settings")
local utils = require("utils")

local mod = {}

-- jumpcutselect
function mod.jumpcutselect()
    function formatChoices(choices)
        choices = utils.reverse(choices)
        formattedChoices = hs.fnutils.imap(choices, function(result)
            return {
                ["text"] = utils.trim(result),
            }
        end)
        return formattedChoices
    end

    local copy = nil
    local current = hs.application.frontmostApplication()
    local copies = settings.get("so.meain.hs.jumpcut") or {}
    local choices = formatChoices(copies)

    local chooser = hs.chooser.new(function(choosen)
        if copy then copy:delete() end
        current:activate()
        hs.eventtap.keyStrokes(choosen)
    end)
    chooser:choices(formatChoices(copies))

    copy = hs.hotkey.bind('', 'return', function()
        local id = chooser:selectedRow()
        local item = choices[id]
        if item then
            chooser:hide()
            trimmed = utils.trim(item.text)
            hs.pasteboard.setContents(trimmed)
            settings.set("so.meain.hs.jumpcutselect.lastselected", trimmed)
            hs.alert.show(trimmed, 1)
            -- hs.notify.show("Jumpcut","Text copied", trimmed)
        else
            hs.alert.show("Nothing to copy", 1)
        end
    end)

    function updateChooser()
        local query = chooser:query()
        local filteredChoices = hs.fnutils.filter(copies, function(result)
            return string.match(result, query)
        end)
        chooser:choices(formatChoices(filteredChoices))
    end

    chooser:queryChangedCallback(updateChooser)
    chooser:searchSubText(false)
    chooser:show()
end

function mod.registerDefaultBindings(mods, key)
    mods = mods or {"cmd", "alt", "ctrl"}
    key = key or "P"
    hs.hotkey.bind(mods, key, mod.jumpcutselect)
end

return mod
