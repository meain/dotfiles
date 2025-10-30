local focusandback = require("focusandback")
local switchscreen = require("switchscreen")
local customshellrun = require("customshellrun")
local utils = require("utils")

local pasteboard = require("hs.pasteboard")

local emacs = "org.gnu.Emacs"
local firefox = "org.mozilla.firefox"
local safari = "com.apple.Safari"
local teams = "com.microsoft.teams2"
local slack = "com.tinyspeck.slackmacgap"
local chrome = "com.google.Chrome"
local obsidian = "md.obsidian"
local cursor = "com.todesktop.230313mzl4w4u92"
local vscodium = "com.vscodium" -- used for sourcegraph cody
local vscode = "com.microsoft.VSCode"
local mail = "com.apple.mail"
local zed = "dev.zed.Zed"
local cal = "com.apple.iCal"

local browser = firefox
local notesApp = obsidian
local editor = emacs

-- key combinations
local fkey = {"alt", "ctrl", "cmd"}
local hyper = {"alt", "ctrl", "cmd", "shift"}

-- alert styling
hs.alert.defaultStyle.strokeWidth = 0
hs.alert.defaultStyle.radius = 0
hs.alert.defaultStyle.textFont = "Monaco"
hs.alert.defaultStyle.textSize = 13
hs.alert.defaultStyle.fadeInDuration = 0.10
hs.alert.defaultStyle.fadeOutDuration = 1
hs.alert.defaultStyle.atScreenEdge = 2 -- need multiple items
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.95 }

function bindFocus(key, app, id)
    hs.hotkey.bind(fkey, key, function()
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
bindFocus("k", firefox, true)
bindFocus("s", slack, true)
bindFocus("l", teams, true)
bindFocus("h", notesApp, true)
bindFocus("j", editor, true)
bindFocus("o", vscode, true)
bindFocus("p", zed, true)
bindFocus("e", mail, true)
bindFocus("y", cal, true)

hs.hotkey.bind(fkey, "i", function() focusandback("ghostty") end)

hs.hotkey.bind(fkey, "w", function()
    local bundleID = hs.application.frontmostApplication():bundleID()
    hs.pasteboard.setContents(bundleID)
    hs.alert("BundleID: " .. bundleID)
end)
hs.hotkey.bind(fkey, "r", hs.reload)
hs.hotkey.bind(fkey, "z", function() hs.application.frontmostApplication():focusedWindow():maximize() end)

hs.hotkey.bind(fkey, 'm', function()
    local win = hs.window.focusedWindow()
    local screen = win:screen()
    win:moveToScreen(screen:next(), true, true)
    utils.moveMouseToScreen(screen:next())
end)

hs.hotkey.bind(hyper, "l", function() hs.alert(customshellrun.run(",linkify")) end)
hs.hotkey.bind(hyper, "y", function() hs.alert(customshellrun.run(",weather-current")) end)
hs.hotkey.bind(hyper, "b", function() customshellrun.run("GUI_PICKER=1 ,bm open") end)
hs.hotkey.bind(hyper, "o", function() hs.alert(customshellrun.run(",open-or-search " .. pasteboard.getContents())) end)
hs.hotkey.bind(hyper, "i",
    function() hs.urlevent.openURL("https://duckduckgo.com/?q=!ducky+" .. pasteboard.getContents()) end)
hs.hotkey.bind(hyper, "u", function() customshellrun.launch(",voice-assistant") end)
hs.hotkey.bind(hyper, "s", function() hs.alert(customshellrun.run("GUI_PICKER=1 ,se", true)) end)

hs.hotkey.bind(fkey, ";", function()
                   _, s = hs.dialog.textPrompt(
                       "Where to?", [[
= Work: iw, it, ia
= Jira: back, dp, cp
= GitHub: gg, gn, cpb, cpp
= Dashboard: dpu, dsa, dd
= Elastic: epu, esa, ed
]], "", "Go")
    local result = customshellrun.run("DATAFILES_PATH=/Users/meain/.config/datafiles ,urlmap " .. s)
    if #result > 0 then
        hs.alert(result) -- show any error
    end
end)

function onBuiltinScreen()
    local scf = hs.mouse.getCurrentScreen():frame()
    local screen_name = hs.screen.mainScreen():name()
    return screen_name == "Color LCD" or screen_name == "Built-in Retina Display"
end

function noteTaker()
    -- Have two windows side by side, one with the current app
    -- taking up the majority of the screen and the other
    -- with the note taking app to the right taking up the rest.
    local ffw = hs.window.focusedWindow()
    hs.application.launchOrFocusByBundleID(notesApp)
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

function customModes(x, y, w, h)
    local chw = hs.window.focusedWindow()
    local chf = chw:frame()

    chf.x = x
    chf.y = y
    chf.w = w
    chf.h = h
    chw:setFrame(chf)
end

function centerMode(w, h)
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

function mainMode(x, y, w, h)
    local ffw = hs.window.focusedWindow()

    local scf = hs.mouse.getCurrentScreen():frame()
    local fff = ffw:frame()

    fff.x = scf.x
    fff.y = scf.y
    fff.w = scf.w * 0.75
    fff.h = scf.h
    ffw:setFrame(fff)
end

function sideMode(x, y, w, h)
    local ffw = hs.window.focusedWindow()

    local scf = hs.mouse.getCurrentScreen():frame()
    local fff = ffw:frame()

    fff.x = scf.x + (scf.w * 0.75)
    fff.y = scf.y
    fff.w = scf.w * 0.25
    fff.h = scf.h
    ffw:setFrame(fff)
end

-- Alight to left or right side of the screen.  Alternate between
-- sides based on where we currently are
local prevSide = "right"
function sidesMode()
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

hs.hotkey.bind(fkey, "n", noteTaker)
hs.hotkey.bind(fkey, "b", function () centerMode(0.6, 0.6) end)
hs.hotkey.bind(fkey, "g", function () centerMode(0.9, 0.9) end)
hs.hotkey.bind(fkey, "v", function () centerMode(0.7, 0.8) end)
hs.hotkey.bind(fkey, "a", mainMode)
hs.hotkey.bind(fkey, "d", sideMode)
hs.hotkey.bind(fkey, "x", sidesMode)

-- cmd+t from anywhere to open a new tab in browser
browsernewtab =
    hs.hotkey.bind(
    {"cmd"},
    "t",
    function()
        local brow = hs.application.applicationsForBundleID(browser)
        if not brow[1]:isFrontmost() then
            hs.application.launchOrFocusByBundleID(browser)
        end
        browsernewtab:disable()
        hs.eventtap.keyStroke({"cmd"}, "t")
        browsernewtab:enable()
    end
)

hs.alert("Hammerspoon loaded!")
