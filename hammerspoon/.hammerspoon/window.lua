local apps = require("apps")
local keys = require("keys")

local window = {}

local function onBuiltinScreen()
    local screen_name = hs.screen.mainScreen():name()
    return screen_name == "Color LCD" or screen_name == "Built-in Retina Display"
end

function window.noteTaker()
    -- Have two windows side by side, one with the current app
    -- taking up the majority of the screen and the other
    -- with the note taking app to the right taking up the rest.
    local ffw = hs.window.focusedWindow()
    hs.application.launchOrFocusByBundleID(apps.notesApp)
    local chw = hs.window.focusedWindow()

    local scf = hs.mouse.getCurrentScreen():frame()
    local fff = ffw:frame()
    local chf = chw:frame()

    local primary_window_size = 0.75
    if onBuiltinScreen() then
        primary_window_size = 0.68
    end

    fff.x = scf.x
    fff.y = scf.y
    fff.w = scf.w * primary_window_size
    fff.h = scf.h
    ffw:setFrame(fff)


    chf.x = scf.x + fff.w
    chf.y = scf.y
    chf.w = scf.w - fff.w
    chf.h = scf.h
    chw:setFrame(chf)
end

function window.customModes(x, y, w, h)
    local chw = hs.window.focusedWindow()
    local chf = chw:frame()

    chf.x = x
    chf.y = y
    chf.w = w
    chf.h = h
    chw:setFrame(chf)
end

function window.centerMode(w, h)
    local currentWindow = hs.window.focusedWindow()
    local screenFrame = hs.mouse.getCurrentScreen():frame()
    local windowFrame = currentWindow:frame()

    local wscaleFactor = w
    local hscaleFactor = h

    windowFrame.x = screenFrame.x + (screenFrame.w * ((1 - wscaleFactor) / 2))
    windowFrame.y = screenFrame.y + (screenFrame.h * ((1 - hscaleFactor) / 2))
    windowFrame.w = screenFrame.w * wscaleFactor
    windowFrame.h = screenFrame.h * hscaleFactor

    currentWindow:setFrame(windowFrame)
end

function window.mainMode(x, y, w, h)
    local ffw = hs.window.focusedWindow()

    local scf = hs.mouse.getCurrentScreen():frame()
    local fff = ffw:frame()

    fff.x = scf.x
    fff.y = scf.y
    fff.w = scf.w * 0.65
    fff.h = scf.h
    ffw:setFrame(fff)
end

function window.sideMode(x, y, w, h)
    local ffw = hs.window.focusedWindow()

    local scf = hs.mouse.getCurrentScreen():frame()
    local fff = ffw:frame()

    fff.x = scf.x + (scf.w * 0.65)
    fff.y = scf.y
    fff.w = scf.w * 0.35
    fff.h = scf.h
    ffw:setFrame(fff)
end

-- Alight to left or right side of the screen.  Alternate between
-- sides based on where we currently are
local prevSide = "right"
function window.sidesMode()
    local currentWindow = hs.window.focusedWindow()
    local screenFrame = hs.mouse.getCurrentScreen():frame()
    local windowFrame = currentWindow:frame()

    -- Did this "prevSide" thing instead of checking if window in the
    -- left because the frame dimensions were incorrect. But now it
    -- seems more useful as we will align the first window to left and
    -- the next one to right.
    if prevSide == "right" then
        -- Align to left side
        windowFrame.x = screenFrame.x
        windowFrame.w = screenFrame.w / 2
        prevSide = "left"
    else
        -- Align to right side
        windowFrame.x = screenFrame.x + (screenFrame.w / 2)
        windowFrame.w = screenFrame.w / 2
        prevSide = "right"
    end

    windowFrame.y = screenFrame.y
    windowFrame.h = screenFrame.h

    currentWindow:setFrame(windowFrame)
end

-- window management bindings
hs.hotkey.bind(keys.fkey, "n", window.noteTaker)
hs.hotkey.bind(keys.fkey, "b", function () window.centerMode(0.6, 0.6) end)
hs.hotkey.bind(keys.fkey, "g", function () window.centerMode(0.9, 0.9) end)
hs.hotkey.bind(keys.fkey, "v", function () window.centerMode(0.7, 0.8) end)
hs.hotkey.bind(keys.fkey, "a", window.mainMode)
hs.hotkey.bind(keys.fkey, "d", window.sideMode)
hs.hotkey.bind(keys.fkey, "x", window.sidesMode)

return window
