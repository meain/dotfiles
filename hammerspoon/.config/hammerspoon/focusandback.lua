local switchscreen = require("switchscreen")

local prev_items = {}

function focusandback(app)
    local toopen = hs.application(app)
    if toopen and toopen:isFrontmost() then
        prev_items[app]:focus()
        prev_items[app] = null
    else
        prev_items[app] = hs.window.focusedWindow()
        hs.application.launchOrFocus(app)
        if app_window ~= nil then
            screen = prev_items[app]:screen()
        end
    end
    local screen = hs.window.focusedWindow():screen()
    if hs.mouse.getCurrentScreen() ~= screen then
        local pt = hs.geometry.rectMidPoint(screen:fullFrame())
        hs.mouse.setAbsolutePosition(pt)
    end
end

return focusandback
