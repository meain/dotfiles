-- Set up logger
local logLevel = 'info'
local log = hs.logger.new('hms', logLevel)

require("jumpcut")
require("autoreload")
require("mousehighlight")
local pasteboard = require("hs.pasteboard")
local customshellrun = require('customshellrun') 


-- Disable animations
hs.window.animationDuration = 0

-- Change alert styles
hs.alert.defaultStyle.strokeWidth = 0
hs.alert.defaultStyle.radius = 0
hs.alert.defaultStyle.textFont = "Operator Mono Book"
hs.alert.defaultStyle.textSize = 20
hs.alert.defaultStyle.fadeInDuration = 0.10
hs.alert.defaultStyle.fadeOutDuration = 1
hs.alert.defaultStyle.atScreenEdge = 2
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.95 }

-- Scren switcher
local switchscreen = require("switchscreen")
hs.hotkey.bind({"ctrl", "cmd"}, "s", function ()
  switchscreen.focusScreen(hs.mouse.getCurrentScreen():next())
end)
hs.hotkey.bind({"ctrl", "cmd", "shift"}, "s", function ()
  switchscreen.focusScreen(hs.mouse.getCurrentScreen():previous())
end)

-- Setup anycomplete
local anycomplete = require("anycomplete")
anycomplete.registerDefaultBindings({"alt"}, 'G')

-- Emoji picker
local emojipicker = require("emojipicker")
emojipicker.registerDefaultBindings({"alt"}, 'E')

-- Clipboard manager
local jcs = require("jumpcutselect")
jcs.registerDefaultBindings({"alt"}, 'P')

-- Music keymaps
hs.hotkey.bind({'alt'}, 'right', function()
  customshellrun.run('/usr/local/bin/cmus-remote -n')
end)
hs.hotkey.bind({'alt'}, 'left', function()
  customshellrun.run('/usr/local/bin/cmus-remote -r')
end)
hs.hotkey.bind({'alt'}, '\\', function()
  customshellrun.run('/usr/local/bin/cmus-remote -u')
end)
hs.hotkey.bind({'alt', 'shift'}, ',', function()
  customshellrun.run('/usr/local/bin/cmus-remote --seek -10')
end)
hs.hotkey.bind({'alt', 'shift'}, '.', function()
  customshellrun.run('/usr/local/bin/cmus-remote --seek +10')
end)

-- Wallpaper keymap
hs.hotkey.bind({'alt', 'shift'}, '\\', function()
  customshellrun.run(BIN .. 'changewall')
end)
hs.hotkey.bind({'ctrl', 'alt', 'shift'}, '\\', function()
  customshellrun.run(BIN .. 'savewall')
  hs.alert('🖼 Wallpaper saved!')
end)

-- Open link or search for item in clipboard
hs.hotkey.bind({'alt', 'shift'}, 'delete', function()
  result = customshellrun.run('/Users/meain/.bin/openorsearch "' ..  pasteboard.getContents() .. '"')
  alert(result)
end)
