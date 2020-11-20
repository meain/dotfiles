local utils = require("utils")
local pasteboard = require("hs.pasteboard")

function slackDrop(filename)
    if (not utils.file_exists(filename)) then
        hs.alert("Item in clipboard is not a valid file")
        return
    end
    hs.alert("Dropping " .. filename)
    hs.eventtap.keyStroke({"cmd"}, "space")
    hs.eventtap.keyStrokes("slack")
    os.execute("sleep " .. tonumber(1))
    hs.eventtap.keyStroke({}, "return")
    os.execute("sleep " .. tonumber(1))
    hs.eventtap.keyStroke({"cmd"}, "t")
    tap =
        hs.eventtap.new(
        {hs.eventtap.event.types.keyUp},
        function(evt)
            print(hs.keycodes.map[evt:getKeyCode()])
            if (hs.keycodes.map[evt:getKeyCode()] == "return") then
                hs.eventtap.keyStroke({}, "return")
                os.execute("sleep " .. tonumber(1))
                hs.eventtap.keyStroke({}, "s")
                hs.eventtap.keyStroke({}, "l")
                hs.eventtap.keyStroke({}, "o")
                hs.eventtap.keyStroke({}, "w")
                hs.eventtap.keyStroke({}, "delete")
                hs.eventtap.keyStroke({}, "delete")
                hs.eventtap.keyStroke({}, "delete")
                hs.eventtap.keyStroke({}, "delete")
                hs.eventtap.keyStroke({"cmd"}, "u")
                hs.eventtap.keyStroke({}, "down")
                hs.eventtap.keyStroke({}, "down")
                hs.eventtap.keyStroke({}, "down")
                hs.eventtap.keyStroke({}, "return")
                hs.eventtap.keyStroke({"cmd", "shift"}, "g")
                hs.eventtap.keyStrokes(filename)
                hs.eventtap.keyStroke({}, "return")
                hs.eventtap.keyStroke({}, "return")
                os.execute("sleep " .. tonumber(2))
                hs.eventtap.keyStroke({}, "return")
                tap:stop()
            end
            return false
        end
    )
    tap:start()
end

return slackDrop
