local apps = require("apps")
local keys = require("keys")
local customshellrun = require("customshellrun")
local utils = require("utils")

local pasteboard = require("hs.pasteboard")

-- utility bindings
hs.hotkey.bind(keys.fkey, "w", function()
    local bundleID = hs.application.frontmostApplication():bundleID()
    hs.pasteboard.setContents(bundleID)
    hs.alert("BundleID: " .. bundleID)
end)
hs.hotkey.bind(keys.fkey, "r", hs.reload)
hs.hotkey.bind(keys.fkey, "z", function() hs.application.frontmostApplication():focusedWindow():maximize() end)

hs.hotkey.bind(keys.fkey, 'm', function()
    local win = hs.window.focusedWindow()
    local screen = win:screen()
    win:moveToScreen(screen:next(), true, true)
    utils.moveMouseToScreen(screen:next())
end)

-- hyper key commands
hs.hotkey.bind(keys.hyper, "l", function() hs.alert(customshellrun.run(",linkify")) end)
hs.hotkey.bind(keys.hyper, "y", function() hs.alert(customshellrun.run(",weather-current")) end)
hs.hotkey.bind(keys.hyper, "b", function() customshellrun.run("GUI_PICKER=1 ,bm open") end)
hs.hotkey.bind(keys.hyper, "o", function() hs.alert(customshellrun.run(",open-or-search " .. pasteboard.getContents())) end)
hs.hotkey.bind(keys.hyper, "i",
    function() hs.urlevent.openURL("https://duckduckgo.com/?q=!ducky+" .. pasteboard.getContents()) end)
hs.hotkey.bind(keys.hyper, "u", function() customshellrun.launch(",voice-assistant") end)
hs.hotkey.bind(keys.hyper, "s", function() hs.alert(customshellrun.run("GUI_PICKER=1 ,se", true)) end)

hs.hotkey.bind(keys.fkey, ";", function()
    _, s = hs.dialog.textPrompt("Go to", [[
= Work: iw, it, ia
= Jira: back, dp, cp
= GitHub: gg, gn, cpb, cpp
= Dashboard: dpu, dsa, dd
= Elastic: epu, esa, ed
]], "", "Go")
    local result = customshellrun.run(",bm go " .. s)
    if #result > 0 then
        hs.alert(result)
    end
end)

-- cmd+t from anywhere to open a new tab in browser
browsernewtab =
    hs.hotkey.bind(
    {"cmd"},
    "t",
    function()
        local brow = hs.application.applicationsForBundleID(apps.browser)
        if not brow[1]:isFrontmost() then
            hs.application.launchOrFocusByBundleID(apps.browser)
        end
        browsernewtab:disable()
        hs.eventtap.keyStroke({"cmd"}, "t")
        browsernewtab:enable()
    end
)
