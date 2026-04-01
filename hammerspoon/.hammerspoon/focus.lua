local apps = require("apps")
local keys = require("keys")
local utils = require("utils")

-- switchscreen

--Predicate that checks if a window belongs to a screen
local function isInScreen(screen, win)
    return win:screen() == screen
end

local function focusScreen(screen)
    --Get windows within screen, ordered from front to back.
    --If no windows exist, bring focus to desktop. Otherwise, set focus on
    --front-most application window.
    local windows = hs.fnutils.filter(hs.window.orderedWindows(), hs.fnutils.partial(isInScreen, screen))
    local windowToFocus = #windows > 0 and windows[1] or hs.window.desktop()
    windowToFocus:focus()

    -- Move mouse to center of screen
    local pt = hs.geometry.rectMidPoint(screen:fullFrame())
    hs.mouse.setAbsolutePosition(pt)
end

-- focusandback

local prev_items = {}

local function focusandback(app)
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

-- bindFocus

local function bindFocus(key, app, id)
    hs.hotkey.bind(keys.fkey, key, function()
        local appInstance = hs.application.get(app)
        if appInstance then
            -- Check if the app is already focused
            if appInstance:isFrontmost() then
                -- Focus on another window of the same app
                local windows = appInstance:allWindows()
                for _, window in ipairs(windows) do
                    if not window:isMinimized() and window:isVisible() and window:id() ~= hs.window.frontmostWindow():id() then
                        window:focus()
                        return
                    end
                end
            else
                appInstance:activate(true) -- Bring all windows of the app to focus
            end
        else
            hs.application.launchOrFocusByBundleID(app)
        end

        utils.moveMouseToCurrentWindowScreen()
    end)
end

-- quick focus
-- bindFocus("o", "/opt/homebrew/opt/emacs-mac/Emacs.app")
bindFocus("k", apps.firefox, true)
bindFocus("s", apps.slack, true)
bindFocus("l", apps.teams, true)
bindFocus("h", apps.notesApp, true)
bindFocus("j", apps.editor, true)
bindFocus("o", apps.vscode, true)
bindFocus("p", apps.zed, true)
bindFocus("e", apps.mail, true)
bindFocus("y", apps.cal, true)

hs.hotkey.bind(keys.fkey, "i", function() focusandback("ghostty") end)
