local prev_items = {}

function focusandback(app)
    local toopen = hs.application(app)
    if prev_items[app] == nil then
        prev_items[app] = hs.window.focusedWindow()
    end

    if toopen and toopen:isFrontmost() then
        prev_items[app]:focus()
        prev_items[app] = null
    else
        hs.application.launchOrFocus(app)
    end
end

return focusandback
