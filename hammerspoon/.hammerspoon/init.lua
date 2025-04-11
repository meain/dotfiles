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
        if id then
            local appInstance = hs.application.get(app)
            if appInstance then
                appInstance:activate(true) -- Bring all windows of the app to focus
            else
                hs.application.launchOrFocusByBundleID(app)
            end
        else
            hs.application.launchOrFocus(app)
        end

        utils.moveMouseToCurrentWindowScreen()
    end)
end

-- quick focus
-- bindFocus("o", "/opt/homebrew/opt/emacs-mac/Emacs.app")
bindFocus("k", "/Applications/Firefox.app")
bindFocus("s", slack, true)
bindFocus("l", teams, true)
bindFocus("h", notesApp, true)
bindFocus("j", editor, true)
bindFocus("o", vscode, true)
bindFocus("p", zed, true)
bindFocus("e", mail, true)
bindFocus("y", cal, true)

hs.hotkey.bind(fkey, "i", function() focusandback("kitty") end)

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
    _, s = hs.dialog.textPrompt("", "Where do you want to go today?", "", "Go")
    customshellrun.run("DATAFILES_PATH=/Users/meain/.config/datafiles ,urlmap " .. s)
end)

function noteTaker()
    -- Have firefox and chrome side by side with firefox on left taking
    -- up 3/4 of the window space
    local ffw = hs.window.focusedWindow()
    hs.application.launchOrFocusByBundleID(notesApp)
    local chw = hs.window.focusedWindow()

    local scf = hs.mouse.getCurrentScreen():frame()
    local fff = ffw:frame()
    local chf = chw:frame()

    fff.x = scf.x
    fff.y = scf.y
    fff.w = scf.w * 0.75
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
    fff.w = scf.w * 0.75 - 20
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

hs.hotkey.bind(fkey, "n", noteTaker)
hs.hotkey.bind(fkey, "b", function () centerMode(0.6, 0.6) end)
hs.hotkey.bind(fkey, "g", function () centerMode(0.8, 0.7) end)
hs.hotkey.bind(fkey, "a", mainMode)
hs.hotkey.bind(fkey, "d", sideMode)

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
