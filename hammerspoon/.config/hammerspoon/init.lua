-- Set up logger
local logLevel = 'info'
local log = hs.logger.new('hms', logLevel)

require("jumpcut")
require("autoreload")
require("mousehighlight")
local utils = require("utils")
local pasteboard = require("hs.pasteboard")
local customshellrun = require('customshellrun') 
local focusandback = require('focusandback') 
local typeout = require('typeout') 

local mailcounter = hs.menubar.new()
mailcounter:setTooltip("No new emails")
mailcounter:setTitle("M")


local taskwarrior = require("taskwarrior")
hs.hotkey.bind({"shift", "alt"}, "t", function()
  taskwarrior.run()
end)

-- mute and unmute mic
hs.loadSpoon("MicMute")
spoon.MicMute:bindHotkeys({
  toggle = { { "shift", "alt" }, "z" }
})

-- Variables
local BIN = os.getenv("HOME") .. '/.bin/'
local prev_foreground_app = nil


-- Disable animations
hs.window.animationDuration = 0

-- Change alert styles
hs.alert.defaultStyle.strokeWidth = 0
hs.alert.defaultStyle.radius = 0
hs.alert.defaultStyle.textFont = "DankMono Nerd Font"
hs.alert.defaultStyle.textSize = 15
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
  hs.alert("📎 " .. utils.trim(pasteboard.getContents()))
end)

-- Music keymaps
showCurrentSong = function()
  song = customshellrun.run(BIN .. 'music/currentsongfull')
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
hs.hotkey.bind({'alt'}, ']', function()
  customshellrun.run(BIN .. 'music/musicspeedup')
end)
hs.hotkey.bind({'alt'}, '[', function()
  customshellrun.run(BIN .. 'music/musicslowdown')
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
  customshellrun.run(BIN .. 'changewall', true)
  hs.alert('🖼 Wallpaper changed!')
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
emailNotify = function(alert)
  if type(alert) == "table" then
    alert = false
  end

  populateMailListing = function()
    mailListing = {}
    unreadcount = utils.linecount(result)
    if (unreadcount > 0) then
      for k,v in pairs(utils.split(result, '\n')) do
        table.insert(mailListing, 1, {title=v})
      end
      table.insert(mailListing, {title="-"})
      table.insert(mailListing, {title=utils.linecount(result) .. ' unread mail', disabled=true})
    else
      table.insert(mailListing, {title='No unread mails', disabled=true})
    end
    return mailListing
  end

  result = customshellrun.run(BIN .. 'unreadsenders | cut -c-120')
  unreadcount = utils.linecount(result)
  if (unreadcount > 0) then
    mailcounter:setTitle(unreadcount)
    mailcounter:setTooltip(result)
    mailcounter:setMenu(populateMailListing(result))
  else
    mailcounter:setTitle("M")
    mailcounter:setTooltip("No new emails")
    mailcounter:setMenu(populateMailListing(result))
  end
  if alert then
    if (string.len(result) > 0) then
      hs.alert("📧 Unread emails\n" .. result)
    else
      hs.alert('📭 No unread emails')
    end
  end
end
hs.pathwatcher.new('/tmp/unreadlocal', emailNotify):start()
hs.hotkey.bind({'alt', 'shift'}, 'e', function()
  emailNotify(true)
  customshellrun.run('/usr/local/bin/tmux refresh-client -S')
end)
hs.hotkey.bind({'ctrl', 'alt', 'shift'}, 'e', function()
  hs.alert('📫 Syncing email')
  customshellrun.run(BIN .. 'mailsync')
end)

hs.hotkey.bind({'alt', 'shift'}, 's', function()
  result = customshellrun.run('/usr/local/bin/task tot|tail -n+4|head -n5')
  hs.alert("🔨 Tasks\n" .. result)
end)

hs.hotkey.bind({'ctrl', 'alt', 'shift'}, 's', function()
  customshellrun.run(BIN .. 'ssq')
end)

hs.hotkey.bind({"cmd", "shift"}, "j", function()
  focusandback("slack")
end)
hs.hotkey.bind({"cmd", "shift"}, "k", function()
  focusandback("firefox")
end)
hs.hotkey.bind({"cmd", "shift"}, "l", function()
  focusandback("alacritty")
end)
hs.hotkey.bind({"cmd", "shift"}, "n", function()
  focusandback("insomnia")
end)
hs.hotkey.bind({"cmd", "shift"}, "g", function()
  focusandback("telegram")
end)

hs.hotkey.bind({"ctrl", "shift", "alt"}, "r", function()
  hs.reload()
end)

hs.hotkey.bind({"ctrl", "shift", "alt"}, "t", function()
  typeout(pasteboard.getContents())
end)
