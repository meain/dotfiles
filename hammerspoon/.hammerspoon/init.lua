-- Set up logger
local logLevel = 'info'
local log = hs.logger.new('hms', logLevel)


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
  switchscreen.focusScreen(hs.window.focusedWindow():screen():next())
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

require "jumpcut"

local jcs = require "jumpcutselect"
jcs.registerDefaultBindings({"alt"}, 'P')

shiftDoublePress = require("shiftDoublePress")
shiftDoublePress.timeFrame = 2

local vimouse = require('vimouse')
vimouse('alt', ';')
