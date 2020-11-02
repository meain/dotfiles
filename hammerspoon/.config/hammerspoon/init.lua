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


-- Variables
local BIN = os.getenv("HOME") .. '/.bin/'


-- Disable animations
hs.window.animationDuration = 0

-- Change alert styles
hs.alert.defaultStyle.strokeWidth = 0
hs.alert.defaultStyle.radius = 0
hs.alert.defaultStyle.textFont = "DankMono Nerd Font"
hs.alert.defaultStyle.textSize = 15
hs.alert.defaultStyle.fadeInDuration = 0.10
hs.alert.defaultStyle.fadeOutDuration = 1
-- hs.alert.defaultStyle.atScreenEdge = 2  -- need multiple items
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.95 }

-- taskwarrior
local taskwarrior = require("taskwarrior")
hs.hotkey.bind({"alt"}, "t", function()
  taskwarrior.run()
end)

-- mute and unmute mic
hs.loadSpoon("MicMute")
spoon.MicMute:bindHotkeys({
  toggle = { { "shift", "alt" }, "z" }
})

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
anycomplete.registerDefaultBindings({"alt", "shift"}, 'G')

-- Emoji picker
local emojipicker = require("emojipicker")
emojipicker.registerDefaultBindings({"alt"}, 'E')


local clipdo = require("clipdo")
clipdo.registerDefaultBindings({"ctrl", "alt", "shift"}, "L")

-- Music keymaps
local showCurrentSong = function()
  local song = customshellrun.run(BIN .. 'music/currentsongfull')
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
  local result = customshellrun.run(BIN .. 'openorsearch "' ..  pasteboard.getContents() .. '"')
  hs.alert(result)
end)

hs.hotkey.bind({'alt', 'shift'}, 'delete', function()
  customshellrun.run(
    '/usr/bin/open \'https://www.google.com/search?q=' ..  pasteboard.getContents() .. '&btnI=I%27m+Feeling+Lucky\''
  )
end)

-- Email watcher
local emailNotify = function(alert)
  if type(alert) == "table" then
    alert = false
  end

  local populateMailListing = function(result)
    local mailListing = {}
    local unreadcount = utils.linecount(result)
    if (unreadcount > 0) then
      for _,v in pairs(utils.split(result, '\n')) do
        table.insert(mailListing, 1, {title=v})
      end
      table.insert(mailListing, {title="-"})
      table.insert(mailListing, {title=utils.linecount(result) .. ' unread mail', disabled=true})
    else
      table.insert(mailListing, {title='No unread mails', disabled=true})
    end
    return mailListing
  end

  local result = customshellrun.run(BIN .. 'unreadsenders | cut -c-120')
  local unreadcount = utils.linecount(result)
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
hs.pathwatcher.new('/Users/meain/.local/share/mail', emailNotify):start()
hs.hotkey.bind({'alt', 'shift'}, 'e', function()
  emailNotify(true)
  customshellrun.run('/usr/local/bin/tmux refresh-client -S')
end)
hs.hotkey.bind({'ctrl', 'alt', 'shift'}, 'e', function()
  hs.alert('📫 Syncing email')
  customshellrun.run(BIN .. 'mailsync')
end)

hs.hotkey.bind({'alt', 'shift'}, 's', function()
  local result = customshellrun.run('/usr/local/bin/task tot|tail -n+4|head -n5')
  hs.alert("🔨 Tasks\n" .. result)
end)

-- quick launch emacs
hs.hotkey.bind({'alt', 'shift'}, 'c', function()
  local output = customshellrun.run('pgrep Emacs', true)
  if string.len(output) > 0 then
    hs.notify.new({title="Starting Emacs", informativeText="Attaching to existing server"}):send()
    customshellrun.run('emacsclient --no-wait -c', true)
  else
    hs.notify.new({title="Starting Emacs", informativeText="Starting new server process"}):send()
    customshellrun.run('Emacs --bg-daemon;emacsclient --no-wait -c', true)
  end
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

-- quick window switching
hs.hotkey.bind({"alt"}, "'", function()
    local emacs = hs.application.find("Emacs")
    local current_app = hs.window.focusedWindow()
    if emacs == nil then
      hs.application.launchOrFocus("firefox")
      return
    end
    if current_app:title() == "Emacs" then
      hs.application.launchOrFocus("firefox")
    else
      emacs:activate()
    end
end)

-- cmd+t from anywhere to open a new tab in browser
browsernewtab = hs.hotkey.bind({"cmd"}, "t", function()
    local current_app = hs.application("firefox")
    if not current_app:isFrontmost() then
        hs.application.launchOrFocus("firefox")
    end
    browsernewtab:disable()
    hs.eventtap.keyStroke({"cmd"}, 't')
    browsernewtab:enable()
end)
commande = hs.hotkey.bind({"cmd"}, "e", function()
    -- remap cmd+e to cmd+t for slack
    local slack = hs.application("slack")
    if slack:isFrontmost() then
      browsernewtab:disable()
      hs.eventtap.keyStroke({"cmd"}, 't')
      browsernewtab:enable()
    else
      commande:disable()
      hs.eventtap.keyStroke({"cmd"}, 'e')
      commande:enable()
    end
end)

hs.hotkey.bind({'alt', 'shift'}, 'P', function()
  hs.alert("📎 " .. utils.trim(pasteboard.getContents()))
end)

hs.hotkey.bind({"ctrl", "shift", "alt"}, "r", function()
  hs.reload()
end)

hs.hotkey.bind({"ctrl", "shift", "alt"}, "t", function()
  typeout(pasteboard.getContents())
end)
