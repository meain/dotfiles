-- Set up logger
local logLevel = 'info'
local log = hs.logger.new('hms', logLevel)


local pasteboard = require("hs.pasteboard")

require("mousehighlight")


-- Disable animations
hs.window.animationDuration = 0

-- Change alert styles
hs.alert.defaultStyle.strokeWidth = 0
hs.alert.defaultStyle.radius = 0
hs.alert.defaultStyle.textFont = "Operator Mono Book"
hs.alert.defaultStyle.textSize = 25
hs.alert.defaultStyle.fadeInDuration = 0.10
hs.alert.defaultStyle.fadeOutDuration = 1
hs.alert.defaultStyle.atScreenEdge = 0
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.95 }

-- Simle key remaps
local switchscreen = require "switchscreen"
hs.hotkey.bind({"ctrl", "cmd"}, "s", function ()
  switchscreen.focusScreen(hs.mouse.getCurrentScreen():next())
end)

hs.hotkey.bind({"ctrl", "cmd", "shift"}, "s", function ()
  switchscreen.focusScreen(hs.mouse.getCurrentScreen():previous())
end)

-- Auto-reload config on change
function reloadConfig(files)
  doReload = false
  for _,file in pairs(files) do
    if file:sub(-4) == ".lua" then
      doReload = true
    end
  end
  if doReload then
    hs.reload()
  end
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.notify.new({title="Hammerspoon", informativeText="Hammerspoon config reloaded!"}):send()

-- Setup anycomplete
local anycomplete = require "anycomplete"
anycomplete.registerDefaultBindings({"alt"}, 'G')

-- Emoji picker
local emojipicker = require "emojipicker"
emojipicker.registerDefaultBindings({"alt"}, 'E')

require "jumpcut"

local jcs = require "jumpcutselect"
jcs.registerDefaultBindings({"alt"}, 'P')

customshellrun = require('customshellrun') 
keyMultiPress = require("keyMultiPress")
keyMultiPress.timeFrame = 1
keyMultiPress.clickCount = 4
keyMultiPress.keyType = 'shift'
keyMultiPress.action = function()
  result = customshellrun.run('/Users/meain/.bin/openorsearch "' ..  pasteboard.getContents() .. '"')
  alert(result)
end

hs.hotkey.bind({'alt'}, '\'', function()
  hs.eventtap.keyStroke({}, 'delete')
end)

hs.hotkey.bind({'alt'}, 'right', function()
  customshellrun.run('/usr/local/bin/cmus-remote -n')
end)

hs.hotkey.bind({'alt'}, 'left', function()
  customshellrun.run('/usr/local/bin/cmus-remote -r')
end)

hs.hotkey.bind({'alt'}, '\\', function()
  customshellrun.run('/usr/local/bin/cmus-remote -u')
end)

hs.hotkey.bind({'alt', 'shift'}, '\\', function()
  customshellrun.run('/Users/meain/.bin/changewall')
end)

hs.hotkey.bind({'alt', 'shift'}, ',', function()
  customshellrun.run('/usr/local/bin/cmus-remote --seek -10')
end)

hs.hotkey.bind({'alt', 'shift'}, '.', function()
  customshellrun.run('/usr/local/bin/cmus-remote --seek +10')
end)
