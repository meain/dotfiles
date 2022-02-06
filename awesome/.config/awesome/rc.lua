-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")

local wibox = require("wibox") -- Widget and layout library
local beautiful = require("beautiful") -- Theme handling library
local naughty = require("naughty") -- Notification library
local menubar = require("menubar")
local mouse = require("mouse")

local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Load Debian menu entries
local debian = require("debian.menu")
local has_fdo, freedesktop = pcall(require, "freedesktop")

-- Error handling (auto fallback)
if awesome.startup_errors then
    naughty.notify(
        {
            preset = naughty.config.presets.critical,
            title = "Oops, there were errors during startup!",
            text = awesome.startup_errors
        }
    )
end
-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal(
        "debug::error",
        function(err)
            -- Make sure we don't go into an endless error loop
            if in_error then
                return
            end
            in_error = true

            naughty.notify(
                {
                    preset = naughty.config.presets.critical,
                    title = "Oops, an error happened!",
                    text = tostring(err)
                }
            )
            in_error = false
        end
    )
end

-- Setup some variables
modkey = "Mod4"
dmenu_config = "-i -fn 'Anka/Coder:size=8' -nb '#000000' -nf '#aaaaaa' -sb '#263238' -sf '#ffffff'"
terminal = "sakura"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Load theme
beautiful.init(gears.filesystem.get_configuration_dir() .. "theme.lua")

-- Enabled layouts
awful.layout.layouts = {awful.layout.suit.tile, awful.layout.suit.spiral, awful.layout.suit.floating}

-- Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
    {
        "hotkeys",
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end
    },
    {"manual", terminal .. " -e man awesome"},
    {"edit config", editor_cmd .. " " .. awesome.conffile},
    {"restart", awesome.restart},
    {
        "quit",
        function()
            awesome.quit()
        end
    }
}

local menu_awesome = {"awesome", myawesomemenu, beautiful.awesome_icon}
local menu_terminal = {"open terminal", terminal}

if has_fdo then
    mymainmenu = freedesktop.menu.build({before = {menu_awesome}, after = {menu_terminal}})
else
    mymainmenu =
        awful.menu(
        {
            items = {
                menu_awesome,
                {"Debian", debian.menu.Debian_menu.Debian},
                menu_terminal
            }
        }
    )
end

mylauncher =
    awful.widget.launcher(
    {
        image = beautiful.awesome_icon,
        menu = mymainmenu
    }
)

menubar.utils.terminal = terminal -- Set the terminal for applications that require it
mytextclock = wibox.widget.textclock("%a %b %d, %I:%M %p") -- Create a textclock widget

function get_file_content_if_exists(filename)
    local content = "..."
    local cfile = io.open(filename, "r")
    if cfile ~= nil then
        content = cfile:read("*a")
    end
    return content
end

function updating_file_textwidget(filename, timeout)
    local w = wibox.widget.textbox(get_file_content_if_exists(filename))
    gears.timer(
        {
            timeout = timeout,
            call_now = true,
            autostart = true,
            callback = function()
                w.text = get_file_content_if_exists(filename)
            end
        }
    )
    return w
end

function updating_cmd_textwidget(command, timeout)
    local w = wibox.widget.textbox("..")
    local refresh = function()
        awful.spawn.easy_async_with_shell(
            command,
            function(result)
                w.text = result
            end
        )
    end
    gears.timer(
        {
            timeout = timeout,
            call_now = true,
            autostart = true,
            callback = function()
                refresh()
            end
        }
    )
    return w, refresh
end

mymailcounter = updating_file_textwidget("/tmp/shellout", 5)
mymailcounter:connect_signal(
    "button::press",
    function(_, _, _, button)
        if button == 1 then
            awful.spawn("zsh -ic ,mail-unread-notify")
        elseif button == 2 then
            awful.spawn('zsh -ic \'notify-send "Syncing mail"\'')
            awful.spawn("zsh -ic ,mail-sync")
        end
    end
)
myaudiostatus, myaudiostatus_refresh =
    updating_cmd_textwidget('amixer -D pulse | awk -F\'[][]\' \'/Left:/ { print "S"$2 }\' | head -n1', 10)
myaudiostatus:connect_signal(
    "button::press",
    function(_, _, _, button)
        if button == 1 then
            awful.util.spawn("amixer -D pulse set Master toggle")
            -- awful.util.spawn("amixer -D pulse set Capture toggle")
            myaudiostatus_refresh()
        end
    end
)

mymicstatus, mymicstatus_refresh =
    updating_cmd_textwidget('amixer -D pulse | awk -F\'[][]\' \'/Left:/ { print "M"$2 }\' | tail -n1', 10)
mymicstatus:connect_signal(
    "button::press",
    function(_, _, _, button)
        if button == 1 then
            awful.util.spawn("amixer -D pulse set Capture toggle")
            mymicstatus_refresh()
        end
    end
)

-- Create a wibox for each screen and add it
local taglist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(t)
            t:view_only()
        end
    ),
    awful.button(
        {},
        3,
        function(t)
            if client.focus then
                client.focus:move_to_tag(t)
            end
        end
    ),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button(
        {},
        2,
        function(t)
            if client.focus then
                client.focus:toggle_tag(t)
            end
        end
    )
)

local tasklist_buttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            if c == client.focus then
                c.minimized = true
            else
                c:emit_signal("request::activate", "tasklist", {raise = true})
            end
        end
    ),
    awful.button(
        {},
        3,
        function()
            awful.menu.client_list({theme = {width = 250}})
        end
    )
)

-- Setting wallpaper
local function set_wallpaper(s)
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end
screen.connect_signal("property::geometry", set_wallpaper) -- Set on geometry change

awful.screen.connect_for_each_screen(
    function(s)
        set_wallpaper(s) -- Wallpaper
        awful.tag({"1", "2", "3", "4", "5", "6", "7", "8", "9"}, s, awful.layout.layouts[1]) -- Each screen has its own tag table.
        s.mypromptbox = awful.widget.prompt() -- Create a promptbox for each screen

        -- Layout switcher
        s.mylayoutbox = awful.widget.layoutbox(s)
        s.mylayoutbox:buttons(
            gears.table.join(
                awful.button(
                    {},
                    1,
                    function()
                        awful.layout.inc(1)
                    end
                ),
                awful.button(
                    {},
                    3,
                    function()
                        awful.layout.inc(-1)
                    end
                )
            )
        )

        -- Create a taglist widget
        s.mytaglist =
            awful.widget.taglist {
            screen = s,
            filter = function(t)
                return t.selected or #t:clients() > 0
            end,
            buttons = taglist_buttons
        }

        -- Create a tasklist widget
        s.mytasklist =
            awful.widget.tasklist {
            screen = s,
            filter = awful.widget.tasklist.filter.currenttags,
            buttons = tasklist_buttons,
            widget_template = {
                {
                    {
                        {
                            id = "text_role",
                            widget = wibox.widget.textbox
                        },
                        layout = wibox.layout.fixed.horizontal
                    },
                    left = 10,
                    right = 10,
                    widget = wibox.container.margin
                },
                id = "background_role",
                widget = wibox.container.background
            }
        }

        -- Create the wibox
        s.mywibox = awful.wibar({position = "top", screen = s})

        -- Add widgets to the wibox
        s.mywibox:setup {
            layout = wibox.layout.align.horizontal,
            {
                -- Left widgets
                layout = wibox.layout.fixed.horizontal,
                mylauncher,
                s.mytaglist,
                s.mypromptbox
            },
            s.mytasklist, -- Middle widget
            {
                -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                wibox.widget.textbox(" "),
                myaudiostatus,
                wibox.widget.textbox(" "),
                mymicstatus,
                wibox.widget.textbox(" * "),
                mymailcounter,
                wibox.widget.textbox(" * "),
                mytextclock,
                wibox.widget.textbox(" "),
                wibox.widget.systray(),
                s.mylayoutbox
            }
        }
    end
)

-- Mouse bindings
root.buttons(
    gears.table.join(
        awful.button(
            {},
            3,
            function()
                mymainmenu:toggle()
            end
        ),
        awful.button({}, 4, awful.tag.viewnext),
        awful.button({}, 5, awful.tag.viewprev)
    )
)

-- Key bindings
function view_next_tag_with_client()
    local initial_tag_index = awful.screen.focused().selected_tag.index
    while (true) do
        awful.tag.viewnext()
        local current_tag = awful.screen.focused().selected_tag
        local current_tag_index = current_tag.index
        if #current_tag:clients() > 0 or current_tag_index == initial_tag_index then
            return
        end
    end
end
function view_prev_tag_with_client()
    local initial_tag_index = awful.screen.focused().selected_tag.index
    while (true) do
        awful.tag.viewprev()
        local current_tag = awful.screen.focused().selected_tag
        local current_tag_index = current_tag.index
        if #current_tag:clients() > 0 or current_tag_index == initial_tag_index then
            return
        end
    end
end
function rotate_screens(direction)
    local current_screen = awful.screen.focused()
    local initial_scren = current_screen
    while (true) do
        awful.screen.focus_relative(direction)
        local next_screen = awful.screen.focused()
        if next_screen == initial_scren then
            return
        end

        local current_screen_tag_name = current_screen.selected_tag.name
        local next_screen_tag_name = next_screen.selected_tag.name

        for _, t in ipairs(current_screen.tags) do
            local fallback_tag = awful.tag.find_by_name(next_screen, t.name)
            local self_clients = t:clients()
            local other_clients

            if not fallback_tag then
                -- if not available, use first tag
                fallback_tag = next_screen.tags[1]
                other_clients = {}
            else
                other_clients = fallback_tag:clients()
            end

            for _, c in ipairs(self_clients) do
                c:move_to_tag(fallback_tag)
            end

            for _, c in ipairs(other_clients) do
                c:move_to_tag(t)
            end
        end
        awful.tag.find_by_name(next_screen, current_screen_tag_name):view_only()
        awful.tag.find_by_name(current_screen, next_screen_tag_name):view_only()
        current_screen = next_screen
    end
end

globalkeys =
    gears.table.join(
    awful.key({modkey}, "g", hotkeys_popup.show_help, {description = "show help", group = "awesome"}),
    awful.key({modkey}, "h", view_prev_tag_with_client, {description = "view previous", group = "tag"}),
    awful.key({modkey}, "l", view_next_tag_with_client, {description = "view next", group = "tag"}),
    awful.key({modkey}, "s", awful.tag.history.restore, {description = "go back", group = "tag"}),
    awful.key({modkey}, "i", awful.tag.history.restore, {description = "go back", group = "tag"}),
    awful.key(
        {modkey},
        "k",
        function()
            awful.client.focus.byidx(1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key(
        {modkey},
        "j",
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    -- Layout manipulation
    awful.key(
        {modkey, "Shift"},
        "j",
        function()
            awful.client.swap.byidx(1)
        end,
        {description = "swap with next client by index", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "k",
        function()
            awful.client.swap.byidx(-1)
        end,
        {description = "swap with previous client by index", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "l",
        function()
            awful.screen.focus_relative(1)
            mouse.highlight()
            gears.timer(
                {
                    timeout = 1,
                    call_now = false,
                    autostart = true,
                    callback = function()
                        mouse.hide()
                        return false
                    end
                }
            )
        end,
        {description = "focus the next screen", group = "screen"}
    ),
    awful.key(
        {modkey, "Shift"},
        "h",
        function()
            awful.screen.focus_relative(-1)
            mouse.highlight()
            gears.timer(
                {
                    timeout = 1,
                    call_now = false,
                    autostart = true,
                    callback = function()
                        mouse.hide()
                        return false
                    end
                }
            )
        end,
        {description = "focus the previous screen", group = "screen"}
    ),
    awful.key(
        {modkey, "Shift"},
        "i",
        awful.client.urgent.jumpto,
        {description = "jump to urgent client", group = "client"}
    ),
    awful.key(
        {modkey},
        "Tab",
        function()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}
    ),
    -- Standard program
    awful.key(
        {modkey},
        "Return",
        function()
            awful.spawn(terminal)
        end,
        {description = "open a terminal", group = "launcher"}
    ),
    awful.key(
        {modkey},
        "e",
        function()
            awful.spawn("emacsclient -nc -a ''")
        end,
        {description = "open an emacs client instance", group = "launcher"}
    ),
    awful.key(
        {modkey, "Shift"},
        "p",
        function()
            awful.spawn("clipmenu " .. dmenu_config)
        end,
        {description = "open an emacs client instance", group = "launcher"}
    ),
    awful.key(
        {modkey},
        ";",
        function()
            awful.spawn("guake")
        end,
        {description = "open guake", group = "launcher"}
    ),
    awful.key(
        {modkey, "Shift"},
        "c",
        function()
            awful.spawn("scrot -s -e 'mv $f ~/Pictures/Screenshots/'")
        end,
        {description = "take screenshot", group = "launcher"}
    ),
    awful.key(
        {modkey, "Shift"},
        "e",
        function()
            awful.spawn("zsh -ic ,mail-unread-notify")
        end,
        {description = "open an emacs client instance", group = "launcher"}
    ),
    awful.key({modkey, "Control"}, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
    awful.key({modkey, "Control"}, "q", awesome.quit, {description = "quit awesome", group = "awesome"}),
    awful.key(
        {modkey, "Shift"},
        "q",
        function()
            awful.spawn("systemctl suspend")
        end,
        {description = "suspend", group = "awesome"}
    ),
    awful.key(
        {modkey},
        "o",
        function()
            awful.tag.incmwfact(0.05)
        end,
        {description = "increase master width factor", group = "layout"}
    ),
    awful.key(
        {modkey},
        "u",
        function()
            awful.tag.incmwfact(-0.05)
        end,
        {description = "decrease master width factor", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "u",
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = "increase the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "o",
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = "decrease the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "u",
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {description = "increase the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "o",
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {description = "decrease the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey},
        "space",
        function()
            awful.layout.inc(1)
        end,
        {description = "select next", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "space",
        function()
            awful.layout.inc(-1)
        end,
        {description = "select previous", group = "layout"}
    ),
    -- Prompt
    awful.key(
        {modkey},
        "r",
        function()
            awful.util.spawn("dmenu_run " .. dmenu_config)
        end,
        {description = "run prompt", group = "launcher"}
    ),
    -- Menubar
    awful.key(
        {modkey, "Shift"},
        "r",
        function()
            menubar.show()
        end,
        {description = "show the menubar", group = "launcher"}
    ),
    awful.key(
        {modkey},
        "x",
        function()
            awful.util.spawn("zsh -ic ,se")
        end,
        {description = "run prompt", group = "launcher"}
    ),
    awful.key(
        {modkey, "Shift"},
        "x",
        function()
            awful.prompt.run {
                prompt = "Run Lua code: ",
                textbox = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
        end,
        {description = "lua execute prompt", group = "awesome"}
    ),
    -- Sound and volume keys
    awful.key(
        {},
        "XF86AudioRaiseVolume",
        function()
            awful.util.spawn("amixer -D pulse set Master '1000+'")
            myaudiostatus_refresh()
        end
    ),
    awful.key(
        {},
        "XF86AudioLowerVolume",
        function()
            awful.util.spawn("amixer -D pulse set Master '1000-'")
            myaudiostatus_refresh()
        end
    ),
    awful.key(
        {},
        "XF86AudioMute",
        function()
            awful.util.spawn("amixer -D pulse set Master toggle")
            myaudiostatus_refresh()
        end
    ),
    awful.key(
        {"Shift"},
        "XF86AudioRaiseVolume",
        function()
            awful.util.spawn("amixer -D pulse set Capture '1000+'")
            mymicstatus_refresh()
        end
    ),
    awful.key(
        {"Shift"},
        "XF86AudioLowerVolume",
        function()
            awful.util.spawn("amixer -D pulse set Capture '1000-'")
            mymicstatus_refresh()
        end
    ),
    awful.key(
        {},
        "XF86AudioMicMute",
        function()
            awful.util.spawn("amixer -D pulse set Capture toggle")
            mymicstatus_refresh()
        end
    ),
    awful.key(
        {},
        "XF86MonBrightnessDown",
        function()
            awful.util.spawn("brightnessctl specific '3-'")
        end
    ),
    awful.key(
        {},
        "XF86MonBrightnessUp",
        function()
            awful.util.spawn("brightnessctl specific '+3'")
        end
    )
)

clientkeys =
    gears.table.join(
    awful.key(
        {modkey},
        "q",
        function(c)
            c:kill()
        end,
        {description = "close", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "f",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}
    ),
    awful.key(
        {modkey},
        "f",
        function(c)
            awful.client.floating.toggle()
            if (awful.client.floating.get() == true) then
                awful.titlebar.show(c)
            else
                awful.titlebar.hide(c)
            end
        end,
        {description = "toggle floating", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "Return",
        function(c)
            c:swap(awful.client.getmaster())
        end,
        {description = "move to master", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "s",
        function(c)
            c:move_to_screen()
        end,
        {description = "move to screen", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "s",
        function()
            rotate_screens(-1)
        end,
        {description = "rotate screens right", group = "screen"}
    ),
    awful.key(
        {modkey, "Control", "Shift"},
        "s",
        function()
            rotate_screens(1)
        end,
        {description = "rotate screens left", group = "screen"}
    ),
    awful.key(
        {modkey},
        "t",
        function(c)
            awful.titlebar.toggle(c)
            c:raise()
        end,
        {description = "toggle titlebars", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "t",
        function(c)
            c.ontop = not c.ontop
        end,
        {description = "toggle keep on top", group = "client"}
    ),
    awful.key(
        {modkey},
        "n",
        function(c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end,
        {description = "minimize", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "n",
        function()
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                c:emit_signal("request::activate", "key.unminimize", {raise = true})
            end
        end,
        {description = "restore minimized", group = "client"}
    ),
    awful.key(
        {modkey},
        "m",
        function(c)
            c.maximized = not c.maximized
            c:raise()
        end,
        {description = "(un)maximize", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "m",
        function(c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end,
        {description = "(un)maximize vertically", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "m",
        function(c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end,
        {description = "(un)maximize horizontally", group = "client"}
    )
)

-- Bind all key numbers to tags.
for i = 1, 9 do
    globalkeys =
        gears.table.join(
        globalkeys,
        -- View tag only.
        awful.key(
            {modkey},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
            end,
            {description = "view tag #" .. i, group = "tag"}
        ),
        -- Toggle tag display.
        awful.key(
            {modkey, "Control"},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            {description = "toggle tag #" .. i, group = "tag"}
        ),
        -- Move client to tag.
        awful.key(
            {modkey, "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            {description = "move focused client to tag #" .. i, group = "tag"}
        ),
        -- Toggle tag on focused client.
        awful.key(
            {modkey, "Control", "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end,
            {description = "toggle focused client on tag #" .. i, group = "tag"}
        )
    )
end

clientbuttons =
    gears.table.join(
    awful.button(
        {},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
        end
    ),
    awful.button(
        {modkey},
        1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.move(c)
        end
    ),
    awful.button(
        {modkey},
        3,
        function(c)
            c:emit_signal("request::activate", "mouse_click", {raise = true})
            awful.mouse.client.resize(c)
        end
    )
)

-- Set keys
root.keys(globalkeys)

-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = clientkeys,
            buttons = clientbuttons,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen
        }
    },
    -- Floating clients.
    {
        rule_any = {
            instance = {"pinentry"},
            class = {"Arandr", "Blueman-manager", "Sxiv", "kdeconnect-indicator", ".blueman-manager-wrapped"},
            name = {"Event Tester"}, --xev
            role = {"pop-up"}
        },
        properties = {floating = true, titlebars_enabled = true}
    },
    {
        rule_any = {class = {"Guake"}},
        properties = {floating = true, border_width = 0}
    },
    -- Make sure does not leave a gap
    {
        rule_any = {class = {"Emacs", "xterm", "Gnome-terminal", "beekeeper-studio"}},
        properties = {size_hints_honor = false}
    }
    -- Set Firefox to always map on the tag named "2" on screen 2
    -- {rule = {class = "Firefox"}, properties = {screen = 2, tag = "2"}}
}

-- Deal with screens being attached/removed
client.connect_signal(
    "manage",
    function(c)
        if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_offscreen(c)
        end
    end
)

client.connect_signal(
    "focus",
    function(c)
        c.border_color = beautiful.border_focus
    end
)
client.connect_signal(
    "unfocus",
    function(c)
        c.border_color = beautiful.border_normal
    end
)

-- Swallowing
function is_terminal(c)
    return (c.class and c.class == "Gnome-terminal") and true or false
end
client.connect_signal(
    "manage",
    function(c)
        if is_terminal(c) then
            return
        end
        local parent_client = awful.client.focus.history.get(c.screen, 1)
        if parent_client and is_terminal(parent_client) then
            parent_client.minimized = true
            c:connect_signal(
                "unmanage",
                function()
                    parent_client.minimized = false
                end
            )
        end
    end
)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
    "request::titlebars",
    function(c)
        -- buttons for the titlebar
        local buttons =
            gears.table.join(
            awful.button(
                {},
                1,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.move(c)
                end
            ),
            awful.button(
                {},
                3,
                function()
                    c:emit_signal("request::activate", "titlebar", {raise = true})
                    awful.mouse.client.resize(c)
                end
            )
        )

        awful.titlebar(c):setup {
            {
                -- Left
                awful.titlebar.widget.iconwidget(c),
                buttons = buttons,
                layout = wibox.layout.fixed.horizontal
            },
            {
                -- Middle
                {
                    -- Title
                    align = "left",
                    widget = awful.titlebar.widget.titlewidget(c)
                },
                buttons = buttons,
                layout = wibox.layout.flex.horizontal
            },
            {
                -- Right
                awful.titlebar.widget.floatingbutton(c),
                awful.titlebar.widget.maximizedbutton(c),
                awful.titlebar.widget.stickybutton(c),
                awful.titlebar.widget.ontopbutton(c),
                awful.titlebar.widget.closebutton(c),
                layout = wibox.layout.fixed.horizontal()
            },
            layout = wibox.layout.align.horizontal
        }
    end
)

awful.spawn.with_shell("pgrep blueman-applet || blueman-applet")
awful.spawn.with_shell("pgrep clipmenud || clipmenud")
awful.spawn.with_shell("pgrep cbatticon || cbatticon")
awful.spawn.with_shell("pgrep guake || guake")
awful.spawn.with_shell("pgrep nm-applet || nm-applet")
awful.spawn.with_shell("pgrep kdeconnect-indicator || kdeconnect-indicator")
