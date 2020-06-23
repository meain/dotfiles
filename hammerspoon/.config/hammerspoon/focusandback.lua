local prev_items = {}

function focusandback(app)
    local toopen = hs.application(app)
    if toopen and toopen:isFrontmost() then
        prev_items[app]:focus()
        prev_items[app] = null
    else
        prev_items[app] = hs.window.focusedWindow()
        hs.application.launchOrFocus(app)
    end
end

return focusandback
