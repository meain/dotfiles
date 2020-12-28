require("autoreload")
require("mousehighlight")
local utils = require("utils")
local pasteboard = require("hs.pasteboard")
local customshellrun = require("customshellrun")
local focusandback = require("focusandback")
local typeout = require("typeout")
local dialog = require("hs.dialog")

local mailcounter = hs.menubar.new()
mailcounter:setTooltip("No new emails")
mailcounter:setTitle("M")

-- Variables
local BIN = os.getenv("HOME") .. "/.bin/"

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
hs.alert.defaultStyle.fillColor = {white = 0, alpha = 0.95}

-- mute and unmute mic
hs.loadSpoon("MicMute")
spoon.MicMute:bindHotkeys(
    {
        toggle = {{"shift", "alt"}, "z"}
    }
)

-- pomodoro
local pomodoro = require("pomodoro")
local menu = hs.menubar.new(true)
pomodoro.setup(menu)

-- Scren switcher
local switchscreen = require("switchscreen")
hs.hotkey.bind(
    {"ctrl", "cmd"},
    "s",
    function()
        switchscreen.focusScreen(hs.mouse.getCurrentScreen():next())
    end
)
hs.hotkey.bind(
    {"ctrl", "cmd", "shift"},
    "s",
    function()
        switchscreen.focusScreen(hs.mouse.getCurrentScreen():previous())
    end
)

-- Setup anycomplete
local anycomplete = require("anycomplete")
anycomplete.registerDefaultBindings({"alt", "shift"}, "G")

-- Emoji picker
local emojipicker = require("emojipicker")
emojipicker.registerDefaultBindings({"alt", "shift"}, "E")

local clipdo = require("clipdo")
clipdo.registerDefaultBindings({"alt", "shift"}, "I")

-- Music keymaps
local showCurrentSong = function()
    local song = customshellrun.run(BIN .. "music/currentsongfull")
    hs.alert(song)
end
hs.hotkey.bind(
    {"alt"},
    "right",
    function()
        customshellrun.run(BIN .. "music/next")
    end
)
hs.hotkey.bind(
    {"alt"},
    "left",
    function()
        customshellrun.run(BIN .. "music/previous")
    end
)
hs.hotkey.bind(
    {"alt"},
    "\\",
    function()
        customshellrun.run(BIN .. "music/playpause")
    end
)
hs.hotkey.bind(
    {"alt"},
    "]",
    function()
        customshellrun.run(BIN .. "music/musicspeedup")
    end
)
hs.hotkey.bind(
    {"alt"},
    "[",
    function()
        customshellrun.run(BIN .. "music/musicslowdown")
    end
)
hs.hotkey.bind(
    {"alt"},
    ",",
    function()
        customshellrun.run(BIN .. "music/seekbackward")
    end
)
hs.hotkey.bind(
    {"alt"},
    ".",
    function()
        customshellrun.run(BIN .. "music/seekforeward")
    end
)
hs.hotkey.bind(
    {"alt", "shift"},
    "m",
    function()
        showCurrentSong()
    end
)

-- Wallpaper keymap
hs.hotkey.bind(
    {"alt", "shift"},
    "\\",
    function()
        customshellrun.run(BIN .. "changewall", true)
        hs.alert("🖼 Wallpaper changed!")
    end
)

-- Open link or search for item in clipboard
hs.hotkey.bind(
    {"alt"},
    "delete",
    function()
        local result = customshellrun.run(BIN .. 'openorsearch "' .. pasteboard.getContents() .. '"')
        hs.alert(result)
    end
)

hs.hotkey.bind(
    {"alt", "shift"},
    "delete",
    function()
        customshellrun.run(
            "/usr/bin/open 'https://www.google.com/search?q=" ..
                pasteboard.getContents() .. "&btnI=I%27m+Feeling+Lucky'"
        )
    end
)

-- Email watcher
local emailNotify = function(alert)
    if type(alert) == "table" then
        alert = false
    end

    local populateMailListing = function(result)
        local mailListing = {}
        local unreadcount = utils.linecount(result)
        if (unreadcount > 0) then
            for _, v in pairs(utils.split(result, "\n")) do
                table.insert(mailListing, 1, {title = v})
            end
            table.insert(mailListing, {title = "-"})
            table.insert(mailListing, {title = utils.linecount(result) .. " unread mail", disabled = true})
        else
            table.insert(mailListing, {title = "No unread mails", disabled = true})
        end
        return mailListing
    end

    local result = customshellrun.run(BIN .. "unreadsenders | cut -c-120")
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
            hs.alert("📭 No unread emails")
        end
    end
end
local emailPathWatcher = hs.pathwatcher.new("/Users/meain/.local/share/mail/.notmuch/xapian", emailNotify)
emailPathWatcher:start()
hs.hotkey.bind(
    {"alt"},
    "e",
    function()
        emailNotify(true)
        customshellrun.run("/usr/local/bin/tmux refresh-client -S")
        emailPathWatcher:stop()
        emailPathWatcher = hs.pathwatcher.new("/Users/meain/.local/share/mail/.notmuch/xapian", emailNotify)
        emailPathWatcher:start()
    end
)
hs.hotkey.bind(
    {"ctrl", "alt"},
    "e",
    function()
        hs.alert("📫 Marking all emails as read")
        customshellrun.run("notmuch tag +notified tag:imbox and tag:unread", true)
        emailNotify(false)
        customshellrun.run("/usr/local/bin/tmux refresh-client -S")
    end
)

hs.hotkey.bind(
    {"alt", "shift"},
    "s",
    function()
        local result = customshellrun.run("/usr/local/bin/task totn|tail -n+4|head -n5")
        hs.alert("🔨 Tasks\n" .. result)
    end
)

-- quick launch emacs
hs.hotkey.bind(
    {"alt", "shift"},
    "c",
    function()
        local output = customshellrun.run("pgrep Emacs", true)
        if string.len(output) > 0 then
            hs.notify.new({title = "Starting Emacs", informativeText = "Attaching to existing server"}):send()
            customshellrun.run("emacsclient --no-wait -c", true)
        else
            hs.notify.new({title = "Starting Emacs", informativeText = "Starting new server process"}):send()
            customshellrun.run("emacsclient -a '' --no-wait -c", true)
        end
    end
)

hs.hotkey.bind(
    {"ctrl", "alt", "shift"},
    "s",
    function()
        customshellrun.run(BIN .. "ssq")
    end
)

hs.hotkey.bind(
    {"cmd", "shift"},
    "j",
    function()
        focusandback("slack")
    end
)
hs.hotkey.bind(
    {"cmd", "shift"},
    "k",
    function()
        focusandback("firefox")
    end
)
hs.hotkey.bind(
    {"cmd", "shift"},
    "l",
    function()
        focusandback("alacritty")
    end
)
hs.hotkey.bind(
    {"cmd", "shift"},
    "n",
    function()
        focusandback("insomnia")
    end
)
hs.hotkey.bind(
    {"cmd", "shift"},
    "g",
    function()
        focusandback("telegram")
    end
)

hs.hotkey.bind(
    {"ctrl", "alt", "shift"},
    "z",
    function()
        for line in io.lines("/Users/meain/.credentials") do
            local first_split = utils.split(line, " ")[2]
            if first_split ~= nil then
                local second_split = utils.split(first_split, "=")[1]
                if second_split == "PERSONAL_ZOOM_LINK" then
                    hs.alert("📞 " .. utils.split(first_split, "'")[2])
                    pasteboard.setContents(utils.split(first_split, "'")[2])
                end
            end
        end
        local currentApp = hs.window.focusedWindow()
        hs.eventtap.keyStroke({"cmd"}, "v")
        hs.application.launchOrFocus("zoom.us")
        hs.eventtap.keyStroke({"cmd", "ctrl"}, "v")
        currentApp:focus()
        os.execute("sleep " .. tonumber(2))
        currentApp:focus()
        os.execute("sleep " .. tonumber(2))
        currentApp:focus()
    end
)

function moveMouseToCurrentWindowScreen()
    local currentApp = hs.window.focusedWindow()
    local screen = currentApp:screen()
    local pt = hs.geometry.rectMidPoint(screen:fullFrame())
    hs.mouse.setAbsolutePosition(pt)
end

-- Quick edit
local quick_edit_app = nil
hs.hotkey.bind(
    {"alt"},
    "`",
    function()
        local emacs = hs.application.find("Emacs")
        local current_app = hs.window.focusedWindow()
        if current_app:title():sub(1, 5) == "Emacs" then
            if quick_edit_app == nil then
                hs.alert("🤔 No edit in progress")
                return
            end
            hs.eventtap.keyStroke({"cmd"}, "a")
            hs.eventtap.keyStroke({"cmd"}, "c")
            hs.eventtap.keyStroke({"alt", "shift"}, ";")
            hs.eventtap.keyStrokes("(kill-buffer)")
            hs.eventtap.keyStroke({}, "return")
            quick_edit_app:focus()
            hs.eventtap.keyStroke({"cmd"}, "a")
            hs.eventtap.keyStroke({"cmd"}, "v")
            quick_edit_app = nil
        else
            quick_edit_app = hs.window.focusedWindow()
            hs.eventtap.keyStroke({"cmd"}, "a")
            hs.eventtap.keyStroke({"cmd"}, "c")
            emacs:activate()
            hs.eventtap.keyStroke({"alt", "shift"}, ";")
            hs.eventtap.keyStrokes("(meain/quick-edit)")
            hs.eventtap.keyStroke({}, "return")
        end
    end
)

-- floating terminal/emacs
floatingterm =
    hs.hotkey.bind(
    {"alt"},
    ";",
    function()
        local current_app = hs.window.focusedWindow()
        if current_app:title():sub(1, 5) == "Emacs" then
            floatingterm:disable()
            hs.eventtap.keyStroke({"alt"}, ";")
            floatingterm:enable()
        else
            hs.eventtap.keyStroke({"cmd"}, ";")
        end
    end
)

-- quick window switching
hs.hotkey.bind(
    {"alt"},
    "'",
    function()
        local emacs = hs.application.find("Emacs")
        local current_app = hs.window.focusedWindow()
        if emacs == nil then
            hs.application.launchOrFocus("firefox")
            return
        end
        if string.match(current_app:title(), "Emacs") then
            hs.application.launchOrFocus("firefox")
        else
            emacs:activate()
        end
        moveMouseToCurrentWindowScreen()
    end
)

-- cmd+t from anywhere to open a new tab in browser
browsernewtab =
    hs.hotkey.bind(
    {"cmd"},
    "t",
    function()
        local firefox = hs.application("firefox")
        if not firefox:isFrontmost() then
            hs.application.launchOrFocus("firefox")
        end
        browsernewtab:disable()
        hs.eventtap.keyStroke({"cmd"}, "t")
        browsernewtab:enable()
    end
)
-- cmd+e from anywhere to select a chat on slack
slackchat =
    hs.hotkey.bind(
    {"cmd"},
    "e",
    function()
        local slack = hs.application("slack")
        if not slack:isFrontmost() then
            hs.application.launchOrFocus("slack")
        end
        browsernewtab:disable()
        hs.eventtap.keyStroke({"cmd"}, "t")
        browsernewtab:enable()
    end
)

hs.hotkey.bind(
    {"ctrl", "shift"},
    "P",
    function()
        hs.alert("📎 " .. utils.trim(pasteboard.getContents()))
    end
)

hs.hotkey.bind(
    {"ctrl", "shift", "alt"},
    "r",
    function()
        hs.reload()
    end
)

hs.hotkey.bind(
    {"ctrl", "shift", "alt"},
    "t",
    function()
        typeout(pasteboard.getContents())
    end
)

hs.hotkey.bind(
    {"alt", "shift"},
    "w",
    function()
        local currentWindow = hs.window.focusedWindow()
        print(currentWindow:title())
        if currentWindow:title() == "Alacritty" then
            currentWindow:move(hs.geometry(940, 40, 480, 835))
        elseif currentWindow:title():sub(1, 5) == "Slack" then
            currentWindow:move(hs.geometry(30, 400, 895, 475))
        else
            if (#hs.screen.allScreens() == 1) then
                if (hs.screen.allScreens()[1]:name() == "SMB2030") then
                    currentWindow:move(hs.geometry(14,44,1567,810))
                else
                    currentWindow:move(hs.geometry(16, 44, 1411, 835))
                end
            else
                currentWindow:move(hs.geometry(1470, -160, 1540, 800))
            end
        end
    end
)

hs.hotkey.bind(
    {"ctrl", "alt", "shift"},
    "w",
    function()
        local currentWindow = hs.window.focusedWindow()
        print(currentWindow:title(), currentWindow:frame())
        if currentWindow:title() == "Alacritty" then
            currentWindow:move(hs.geometry(31, 40, 893, 347))
        else
            currentWindow:move(hs.geometry(184, 184, 1077, 512))
        end
    end
)

hs.hotkey.bind(
    {"cmd", "shift"},
    ";",
    function()
        hs.focus() -- this is needed for the textPrompt to have focus
        local button, command = dialog.textPrompt("Command", "Enter command to exec", "", "Run", "Cancel")
        if button == "Run" then
            customshellrun.run("tmux send-keys -t master:1 C-c", true)
            customshellrun.run("tmux send-keys -t master:1 '" .. command .. "'", true)
            customshellrun.run("tmux send-keys -t master:1 Enter", true)
        end
    end
)

local canvas = nil
local canvascommand = "/usr/local/bin/task tot | tail -n+4 | sed '$ d'"
function showOutputInCanvas()
    local hcaltitlecolor = {red = 1, blue = 1, green = 1, alpha = 0.5}
    local cscreen = hs.screen.mainScreen()
    local cres = cscreen:fullFrame()
    if canvas ~= nil then
        canvas:hide()
    end
    canvas =
        hs.canvas.new(
        {
            x = 30,
            y = 50,
            w = 1000,
            h = 500
        }
    )

    canvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces)
    canvas:level(hs.canvas.windowLevels.desktopIcon)

    local result = customshellrun.run(canvascommand)
    canvas[1] = {
        id = "hcal_title",
        type = "text",
        text = result,
        textFont = "DankMono Nerd Font",
        textSize = 10,
        textColor = hcaltitlecolor,
        textAlignment = "left"
    }
    canvas:show()
end
function updateOutputCanvas(result)
    local result = customshellrun.run(canvascommand)
    canvas[1].text = result
end
showOutputInCanvas()

-- taskwarrior
local taskwarrior = require("taskwarrior")
local canvastimer = nil
hs.hotkey.bind(
    {"alt"},
    "t",
    function()
        updateOutputCanvas()
        if canvastimer == nil then
            canvastimer =
                hs.timer.doEvery(
                100,
                function()
                    updateOutputCanvas()
                end
            )
            canvastimer:setNextTrigger(10)
        else
            canvastimer:start()
            canvastimer:setNextTrigger(10)
        end
        -- hs.alert(customshellrun.run(BIN .. 'task-choose', true))
        taskwarrior.run()
    end
)
