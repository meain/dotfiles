require("autoreload")
require("mousehighlight")
local utils = require("utils")
local pasteboard = require("hs.pasteboard")
local customshellrun = require("customshellrun")
local focusandback = require("focusandback")
local typeout = require("typeout")
local dialog = require("hs.dialog")
local json = require("json")

local mailcounter = hs.menubar.new()
mailcounter:setTooltip("No new emails")
mailcounter:setTitle("M")

-- Variables
local BIN = os.getenv("HOME") .. "/.bin/"
local browser = "firefox"
local editor = "Emacs"
local terminal = "alacritty"

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
        customshellrun.run(BIN .. "changewall")
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

    function table.slice(tbl, first, last, step)
        local sliced = {}
        for i = first or 1, last or #tbl, step or 1 do
            sliced[#sliced + 1] = tbl[i]
        end
        return sliced
    end

    local populateMailListing = function(result)
        local mailListing = {}
        local unreadcount = utils.linecount(result)
        if (unreadcount > 0) then
            for _, v in pairs(utils.split(result, "\n")) do
                local eid = utils.split(v, " ")[2]
                local vv = table.concat(table.slice(utils.split(v, " "), 3, #utils.split(v, " "), 1), " ")
                table.insert(
                    mailListing,
                    1,
                    {
                        title = vv,
                        fn = function()
                            local jout = customshellrun.run("notmuch show --format json " .. eid)
                            local jse = json.decode(jout)
                            local mailcontent = ""
                            local unreadcount = 0
                            mailcontent =
                                mailcontent .. utils.dropEmailFooter(jse[1][1][1]["body"][1]["content"][1]["content"])
                            mailcontent = mailcontent .. "\n         ==========================\n"
                            local messages = jse[1][1][2]
                            for i, message in ipairs(messages) do
                                if utils.isin(message[1]["tags"], "unread") then
                                    unreadcount = unreadcount + 1
                                    mailcontent =
                                        mailcontent ..
                                        utils.dropEmailFooter(message[1]["body"][1]["content"][1]["content"])
                                    mailcontent = mailcontent .. "\n         --------------------------\n"
                                end
                            end
                            local pressedButton =
                                hs.dialog.blockAlert(
                                vv .. "(" .. unreadcount .. " unread)",
                                mailcontent,
                                "Mark Read",
                                "OK"
                            )
                            if pressedButton == "Mark Read" then
                                customshellrun.run("notmuch tag -unread " .. eid)
                            end
                        end
                    }
                )
            end
            table.insert(mailListing, {title = "-"})
            table.insert(mailListing, {title = utils.linecount(result) .. " unread mail", disabled = true})
        else
            table.insert(mailListing, {title = "No unread mails", disabled = true})
        end
        return mailListing
    end

    local result = customshellrun.run(BIN .. "unreadsenders | cut -c-120")
    local unreadcount = customshellrun.run(BIN .. "mailcounter")
    mailcounter:setTitle(unreadcount)
    mailcounter:setTooltip(result)
    mailcounter:setMenu(populateMailListing(result))
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
        customshellrun.run("tmux refresh-client -S")
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
        customshellrun.run("tmux refresh-client -S")
    end
)

hs.hotkey.bind(
    {"alt", "shift"},
    "s",
    function()
        local result = customshellrun.run("task totn|tail -n+4|head -n5")
        hs.alert("🔨 Tasks\n" .. result)
    end
)

-- quick launch text editor
hs.hotkey.bind(
    {"alt", "shift"},
    "c",
    function()
        hs.notify.new(
            {title = "Starting editor", informativeText = "Just give it a moment while we pick the best editor"}
        ):send()
        customshellrun.run("emacsclient -a '' --no-wait -c", true)
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
        focusandback(editor)
    end
)
hs.hotkey.bind(
    {"cmd", "shift"},
    "k",
    function()
        focusandback(browser)
    end
)
hs.hotkey.bind(
    {"cmd", "shift"},
    "h",
    function()
        focusandback(terminal)
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
    {"ctrl", "alt", "shift"},
    "z",
    function()
        local currentApp = hs.window.focusedWindow()
        hs.application.launchOrFocus("zoom.us")
        hs.eventtap.keyStroke({"cmd", "ctrl"}, "v")
        os.execute("sleep " .. tonumber(7))
        currentApp:focus()
        hs.eventtap.keyStroke({"cmd"}, "v")
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
        local emacs = hs.application.find(editor)
        local current_app = hs.window.focusedWindow()
        if current_app:title():sub(1, 5) == editor then
            if quick_edit_app == nil then
                hs.alert("🤔 No edit in progress")
                return
            end
            hs.eventtap.keyStroke({"alt", "shift"}, ";")
            hs.eventtap.keyStrokes("(meain/quick-edit-end)")
            hs.eventtap.keyStroke({}, "return")
            quick_edit_app:focus()
            os.execute("sleep " .. tonumber(1))
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
        if current_app:title():sub(1, 5) == editor then
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
        local emacs = hs.application.find(editor)
        local current_app = hs.window.focusedWindow()
        if current_app == nil then
            emacs:activate()
        end
        if emacs == nil then
            hs.application.launchOrFocus(browser)
            return
        end
        if string.match(current_app:title():sub(1, 5), editor) then
            hs.application.launchOrFocus(browser)
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
        local firefox = hs.application(browser)
        if not firefox:isFrontmost() then
            hs.application.launchOrFocus(browser)
        end
        browsernewtab:disable()
        hs.eventtap.keyStroke({"cmd"}, "t")
        browsernewtab:enable()
    end
)

-- cmd+e to open chat picker in slack/element
slackchat =
    hs.hotkey.bind(
    {"cmd"},
    "e",
    function()
        local slack = hs.application("slack")
        local element = hs.application("element")
        local current_app = hs.window.focusedWindow()
        if current_app:title():sub(1, 7) == "Element" or (slack == nil and element ~= nil) then
            if not element:isFrontmost() then
                hs.application.launchOrFocus("element")
            end
        else
            if not slack:isFrontmost() then
                hs.application.launchOrFocus("slack")
            end
        end
        browsernewtab:disable()
        hs.eventtap.keyStroke({"cmd"}, "k")
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

function getMainWindowPosition()
    local mainScreen = hs.screen.allScreens()[1]
    if (#hs.screen.allScreens() == 2) then
        mainScreen = hs.screen.allScreens()[2]
    end
    local position = mainScreen:frame()
    if (mainScreen:name() == "SMB2030") then
        return hs.geometry(position.x + 10, position.y + 10, position.w - 20, position.h - 50)
    else
        return hs.geometry(position.x + 10, position.y + 10, position.w - 20, position.h - 20)
    end
end

hs.hotkey.bind(
    {"alt", "shift"},
    "w",
    function()
        local currentWindow = hs.window.focusedWindow()
        local wPosition = getMainWindowPosition()
        local title = currentWindow:title()
        -- "Master" is iTerm
        if (title == "Alacritty" or title == "Master") then
            currentWindow:move(hs.geometry(940, 40, 480, 840))
        elseif
            title:sub(1, 5) == "Slack" or title:sub(1, 7) == "Element" or title:sub(1, 8) == "Hydrogen" or
                title:sub(title:len() - 6, title:len()) == "Discord"
         then
            currentWindow:move(hs.geometry(10, 390, 920, 500))
        else
            currentWindow:move(wPosition)
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
        local prev_window = hs.window.focusedWindow()
        hs.focus() -- this is needed for the textPrompt to have focus
        local button, command =
            dialog.textPrompt("Command input", "Enter command to exec in master window", "", "Run", "Cancel")
        if button == "Run" then
            customshellrun.run("tmux send-keys -t master:1 C-c")
            customshellrun.run("tmux send-keys -t master:1 '" .. command .. "'")
            customshellrun.run("tmux send-keys -t master:1 Enter")
        end
        if prev_window then
            prev_window:focus()
        end
    end
)

local canvas = nil
local canvascommand = "task tiny | tail -n+4 | sed '$ d'"
local hcaltitlecolor = {red = 255, blue = 255, green = 255, alpha = 0.8}
function ShowOutputInCanvas()
    if canvas ~= nil then
        canvas:hide()
    end
    canvas =
        hs.canvas.new(
        {
            x = 30,
            y = 50,
            w = 2000,
            h = 500
        }
    )

    canvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces)
    canvas:level(hs.canvas.windowLevels.desktopIcon)

    local result = customshellrun.run(canvascommand)
    canvas[1] = {
        id = "taskwarriror-display",
        type = "text",
        text = result,
        textFont = "DankMono Nerd Font",
        textSize = 13,
        textColor = hcaltitlecolor,
        textAlignment = "left"
    }
    canvas:show()
end
function UpdateOutputCanvas()
    local result = customshellrun.run(canvascommand)
    canvas[1].text = result
end
ShowOutputInCanvas()

-- Date canvas
local datecanvas = nil
function PaintDateCanvas()
    if datecanvas ~= nil then
        datecanvas:hide()
    end
    datecanvas =
        hs.canvas.new(
        {
            x = 10,
            y = 630,
            w = 600,
            h = 600
        }
    )

    datecanvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces)
    datecanvas:level(hs.canvas.windowLevels.desktopIcon)
    local diffDate = os.difftime(os.time(), os.time({year = 1996, month = 8, day = 13}))
    local daysSince = math.floor(diffDate / (24 * 60 * 60))

    datecanvas[1] = {
        id = "date-display",
        type = "text",
        text = daysSince,
        textFont = "Operator Mono",
        textSize = 200,
        textColor = hcaltitlecolor,
        textAlignment = "left"
    }
    datecanvas:show()
end
PaintDateCanvas()
hs.timer.doEvery(
    -- Ideally just every day would do, but doing it ever hour as we
    -- might not start ar the right hour.
    60 * 60,
    function()
        PaintDateCanvas()
    end
)

-- taskwarrior
local taskwarrior = require("taskwarrior")
local canvastimer = nil
hs.hotkey.bind(
    {"alt"},
    "t",
    function()
        UpdateOutputCanvas()
        if canvastimer == nil then
            canvastimer =
                hs.timer.doEvery(
                100,
                function()
                    UpdateOutputCanvas()
                end
            )
            canvastimer:setNextTrigger(10)
        else
            canvastimer:start()
            canvastimer:setNextTrigger(10)
        end
        -- hs.alert(customshellrun.run(BIN .. 'task-choose'))
        taskwarrior.run()
    end
)

-- workspace login
local workspaceLogin = require("workspacelogin")
hs.hotkey.bind(
    {"cmd", "alt"},
    "w",
    function()
        workspaceLogin()
    end
)

hs.hotkey.bind({"ctrl", "alt"}, "h", hs.toggleConsole)
hs.notify.new({title = "Hammerspoon loaded!", informativeText = "You are now ready to start clicking"}):send()
