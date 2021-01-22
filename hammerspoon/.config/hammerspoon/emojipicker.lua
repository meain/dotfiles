local utils = require("utils")
local settings = require("hs.settings")

local mod = {}

local file = io.open("emojis.txt", "r")
local emojis = {}
for line in file:lines() do
    table.insert(emojis, 1, line)
end

function utf8.sub(s, i, j)
    i = utf8.offset(s, i)
    j = utf8.offset(s, j + 1) - 1
    return string.sub(s, i, j)
end

-- Emojipicker
function mod.emojipicker()
    function formatChoices(choices)
        choices = utils.reverse(choices)
        formattedChoices =
            hs.fnutils.imap(
            choices,
            function(result)
                return {
                    ["text"] = utils.trim(result)
                }
            end
        )
        return formattedChoices
    end

    local copy = nil
    local current = hs.application.frontmostApplication()
    local choices = formatChoices(emojis)

    local chooser =
        hs.chooser.new(
        function(choosen)
            if copy then
                copy:delete()
            end
            current:activate()
            hs.eventtap.keyStrokes(choosen)
        end
    )
    chooser:width(69)
    chooser:placeholderText("Pick emoji")
    chooser:choices(formatChoices(emojis))

    copy =
        hs.hotkey.bind(
        "",
        "return",
        function()
            local query = chooser:query()
            local filteredChoices =
                hs.fnutils.filter(
                emojis,
                function(result)
                    return string.match(string.lower(result), string.lower(query))
                end
            )
            local id = chooser:selectedRow()
            local item = formatChoices(filteredChoices)[id]
            if item then
                chooser:hide()
                trimmed = utf8.sub(item.text, 1, 1)
                hs.pasteboard.setContents(trimmed)
                settings.set("so.meain.hs.jumpcutselect.lastselected", trimmed) -- need to make sure it does not get copied to clipboard
                hs.alert.show(item.text, 1)
            else
                hs.alert.show("Nothing to copy", 1)
            end
        end
    )

    function updateChooser()
        local query = chooser:query()
        local filteredChoices =
            hs.fnutils.filter(
            emojis,
            function(result)
                return string.match(string.lower(result), string.lower(query))
            end
        )
        chooser:choices(formatChoices(filteredChoices))
    end

    chooser:queryChangedCallback(updateChooser)
    chooser:searchSubText(false)
    chooser:show()
end

function mod.registerDefaultBindings(mods, key)
    mods = mods or {"cmd", "alt", "ctrl"}
    key = key or "E"
    hs.hotkey.bind(mods, key, mod.emojipicker)
end

return mod
