-- Set up logger
local logLevel = 'info'
local log = hs.logger.new('hms', logLevel)

require("jumpcut")
require("autoreload")
require("mousehighlight")
local utils = require("utils")
local pasteboard = require("hs.pasteboard")
local customshellrun = require('customshellrun') 


local mailcounter = hs.menubar.new()
mailcounter:setTooltip("No new emails")
mailcounter:setTitle("M")

-- Variables
local BIN = os.getenv("HOME") .. '/.bin/'


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
hs.hotkey.bind({'alt', 'shift'}, 'P', function()
  hs.alert("📎 " .. pasteboard.getContents())
end)

-- Music keymaps
showCurrentSong = function()
  song = customshellrun.run(BIN .. 'music/currentsong')
  hs.alert(song)
end
hs.hotkey.bind({'alt'}, 'right', function()
  customshellrun.run(BIN .. 'music/next')
end)
hs.hotkey.bind({'alt'}, 'left', function()
  customshellrun.run(BIN .. 'music/previous')
end)
hs.hotkey.bind({'alt'}, '\\', function()
  customshellrun.run(BIN .. 'music/playpause')
end)
hs.hotkey.bind({'alt'}, ',', function()
  customshellrun.run(BIN .. 'music/seekbackward')
end)
hs.hotkey.bind({'alt'}, '.', function()
  customshellrun.run(BIN .. 'music/seekforeward')
end)
hs.hotkey.bind({'alt', 'shift'}, 'm', function()
  showCurrentSong()
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
hs.hotkey.bind({'alt'}, 'delete', function()
  result = customshellrun.run(BIN .. 'openorsearch "' ..  pasteboard.getContents() .. '"')
  hs.alert(result)
end)

hs.hotkey.bind({'alt', 'shift'}, 'delete', function()
  customshellrun.run('/usr/bin/open \'https://www.google.com/search?q=' ..  pasteboard.getContents() .. '&btnI=I%27m+Feeling+Lucky\'')
end)

-- Email watcher
emailNotify = function(paths, flags)
  local sound = false
  if type(paths) == "table" then
    -- called using filewatcher
    sound = true
  end

  result = customshellrun.run(BIN .. 'unreadsendersmini')
  unreadcount = utils.linecount(result)
  if (unreadcount > 0) then
    mailcounter:setTitle(unreadcount)
    mailcounter:setTooltip(result)
  else
    mailcounter:setTitle("M")
    mailcounter:setTooltip("No new emails")
  end
  if (string.len(result) > 0) then
    if sound then
      hs.sound.getByName(hs.sound.systemSounds()[9]):play()
    end
    hs.alert("📧 Unread emails\n" .. result)
  else
    -- useful only when called outside of pathwatcher
    hs.alert('📭 No unread emails')
  end
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.local/share/mail/meain/INBOX/new/", emailNotify):start()
hs.hotkey.bind({'alt', 'shift'}, 'e', function()
  emailNotify()
  customshellrun.run('/usr/local/bin/tmux refresh-client -S')
end)
hs.hotkey.bind({'ctrl', 'alt', 'shift'}, 'e', function()
  hs.alert('📫 Syncing email')
  customshellrun.run(BIN .. 'mailsync')
end)
