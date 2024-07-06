local focusandback = require("focusandback")
local switchscreen = require("switchscreen")
local customshellrun = require("customshellrun")
local utils = require("utils")

local pasteboard = require("hs.pasteboard")

local editor = "org.gnu.Emacs"
local browser = "org.mozilla.firefox"
local safari = "com.apple.Safari"
local teams = "com.microsoft.teams2"
local slack = "com.tinyspeck.slackmacgap"
local chrome = "com.google.Chrome"

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
hs.alert.defaultStyle.atScreenEdge = 2  -- need multiple items
hs.alert.defaultStyle.fillColor = {white = 0, alpha = 0.95}

function bindFocus(key, app, id)
    hs.hotkey.bind(fkey, key, function()
            if id then
                hs.application.launchOrFocusByBundleID(app)
            else
                hs.application.launchOrFocus(app)
            end

            utils.moveMouseToCurrentWindowScreen()
        end)
end

-- quick focus
bindFocus("j", "/opt/homebrew/opt/emacs-mac/Emacs.app")
-- bindFocus("k", "/Users/meain/Applications/Home Manager Apps/Firefox.app")
bindFocus("k", "/Applications/Firefox.app")
bindFocus("s", slack, true)
bindFocus("l", teams, true)
bindFocus("h", chrome, true)

hs.hotkey.bind(fkey, "i", function() focusandback("wezterm") end)
-- hs.hotkey.bind(fkey, "n", function() focusandback("logseq") end)

hs.hotkey.bind(fkey, "w", function() hs.alert("BundleID: "..hs.application.frontmostApplication():bundleID()) end)
hs.hotkey.bind(fkey, "r", hs.reload)
hs.hotkey.bind(fkey, "z", function() hs.application.frontmostApplication():focusedWindow():maximize() end)

hs.hotkey.bind(fkey, 'm', function()
  local win = hs.window.focusedWindow()
  local screen = win:screen()
  win:moveToScreen(screen:next(), true, true)
end)

hs.hotkey.bind(hyper, "l", function() hs.alert(customshellrun.run(",linkify")) end)
hs.hotkey.bind(hyper, "y", function() hs.alert(customshellrun.run(",weather-current")) end)
hs.hotkey.bind(hyper, "b", function() customshellrun.run("GUI_PICKER=1 ,bm open") end)
hs.hotkey.bind(hyper, "o", function() hs.alert(customshellrun.run(",open-or-search " .. pasteboard.getContents())) end)
hs.hotkey.bind(hyper, "i", function() customshellrun.run("GUI_PICKER=1 ,lucky-search") end)
hs.hotkey.bind(hyper, "u", function() customshellrun.run(",mail-unread-notify", true) end)
hs.hotkey.bind(hyper, "s", function() hs.alert(customshellrun.run("GUI_PICKER=1 ,se", true)) end)

hs.hotkey.bind(fkey, ";", function()
                  _, s = hs.dialog.textPrompt("", "Where do you want to go today?", "", "Go")
                  customshellrun.run("DATAFILES_PATH=/Users/meain/.config/datafiles ,urlmap " .. s)
end)

function noteTaker()
   -- Have firefox and chrome side by side with firefox on left taking
   -- up 3/4 of the window space
   local ffw = hs.window.focusedWindow()
   hs.application.launchOrFocus("google chrome")
   local chw = hs.window.focusedWindow()

   local scf = hs.mouse.getCurrentScreen():frame()
   local fff =  ffw:frame()
   local chf = chw:frame()

   fff.x = scf.x
   fff.y = scf.y
   fff.w = scf.w * 0.75 - 20
   fff.h = scf.h
   ffw:setFrame(fff)


   chf.x = scf.x + fff.w
   chf.y = scf.y
   chf.w = scf.w * 0.25
   chf.h = scf.h
   chw:setFrame(chf)
end

function customModes(x,y,w,h)
   local chw = hs.window.focusedWindow()
   local chf = chw:frame()

   chf.x = x
   chf.y = y
   chf.w = w
   chf.h = h
   chw:setFrame(chf)
end

function miniMode()
   customModes(700, 200, 500, 750)
end

function centerMode()
   local currentWindow = hs.window.focusedWindow()
   local screenFrame = hs.mouse.getCurrentScreen():frame()
   local windowFrame = currentWindow:frame()

   local wscaleFactor = 0.7
   local hscaleFactor = 0.6

   windowFrame.x = screenFrame.x + (screenFrame.w * ((1 - wscaleFactor)/2))
   windowFrame.y = screenFrame.y + (screenFrame.h * ((1 - hscaleFactor)/2))
   windowFrame.w = screenFrame.w * wscaleFactor
   windowFrame.h = screenFrame.h * hscaleFactor

   currentWindow:setFrame(windowFrame)
end

function mainMode(x,y,w,h)
   local ffw = hs.window.focusedWindow()

   local scf = hs.mouse.getCurrentScreen():frame()
   local fff =  ffw:frame()

   fff.x = scf.x
   fff.y = scf.y
   fff.w = scf.w * 0.75 - 20
   fff.h = scf.h
   ffw:setFrame(fff)
end

function sideMode(x,y,w,h)
   local ffw = hs.window.focusedWindow()

   local scf = hs.mouse.getCurrentScreen():frame()
   local fff =  ffw:frame()

   fff.x = scf.x + (scf.w * 0.75)
   fff.y = scf.y
   fff.w = scf.w * 0.25
   fff.h = scf.h
   ffw:setFrame(fff)
end

hs.hotkey.bind(fkey, "n", noteTaker)
hs.hotkey.bind(fkey, "b", miniMode)
hs.hotkey.bind(fkey, "g", centerMode)
hs.hotkey.bind(fkey, "a", mainMode)
hs.hotkey.bind(fkey, "d", sideMode)

hs.alert("Hammerspoon loaded!")
