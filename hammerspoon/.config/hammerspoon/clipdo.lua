local mod = {}

local pasteboard = require("hs.pasteboard")
local styledtext = require("hs.styledtext")

local customshellrun = require("customshellrun")
local utils = require("utils")

local doables = {
    {"Add item to grourcer list", "add-to-gourcer"},
    {"Base64 encode text", "b64 encode"},
    {"Base64 decode text", "b64 decode"},
    {"Submit gource entry", "gource"},
    {"Clone folder and start tempg session", "tempg"},
    {"Search text on melpa", "search melpa"},
    {"Search text on github", "search github"},
    {"Search text on npm", "search npm"},
    {"Open file or url", "open"},
    {"Open or search for file/url", "openorsearch"}
}

function mod.clipdo()
    local function formatChoices(choices)
        print("choices:", choices)
        local formattedChoices = {}
        for _, item in pairs(choices) do
            local text =
                styledtext.new(
                item[1],
                {
                    font = {size = 16, name = "DankMonoNerdFontComplete-Regular"}
                }
            )
            table.insert(formattedChoices, {["text"] = text, ["command"] = item[2]})
        end
        return formattedChoices
    end

    local link = utils.trim(pasteboard.getContents())
    local chooser =
        hs.chooser.new(
        function(chosen)
            hs.alert("🧧 " .. chosen.text .. " '" .. link .. "'")
            print(chosen.command .. " '" .. link .. "'")
            local result = customshellrun.run(chosen.command .. " '" .. link .. "'", true)
            if (result == nil or result == "") then
                hs.alert("-- no output --")
            else
                hs.alert(result)
            end
        end
    )
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
