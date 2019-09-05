-- vim: set foldmethod=marker :

-- {{{ Boilerplate

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
local sharedtags = require("sharedtags")
local math = require("math")
local solarized = require("solarized")

-- }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("~/.config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "kitty"
editor = os.getenv("EDITOR") or "vi"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"
manipkey = "#66"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
}

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wallpaper
local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Set for every screen
awful.screen.connect_for_each_screen(set_wallpaper)

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)
-- }}}

-- {{{ Wibar

-- Primary screen -- gets the wibox
primary_screen = screen[math.min(screen:count(), 2)]

-- Utility screen -- browser and email
utility_screen = screen[math.min(screen:count(), 3)]

-- {{{ Textclock widget
textclock = wibox.widget.textclock("%a %d %b %Y, %I:%M %p %Z")
-- }}}

-- {{{ Battery widget
local battery_widget = require("battery")
-- }}}

-- {{{ Brightness widget
local brightness_widget = require("brightness")
-- }}}

-- {{{ Network widget
local network_widget = require("network")
-- }}}

-- {{{ Mode indicator widget
mode_widget = wibox.widget.textbox()
mode_widget.markup = "(???)"
-- }}}

-- {{{ Tag list widget
local tags = sharedtags({
    { name = 1, layout = awful.layout.layouts[1] },
    { name = 2, layout = awful.layout.layouts[1] },
    { name = 3, layout = awful.layout.layouts[1] },
    { name = 4, layout = awful.layout.layouts[1] },
    { name = 5, layout = awful.layout.layouts[1] },
    { name = 6, layout = awful.layout.layouts[1] },
    { name = 7, layout = awful.layout.layouts[1] },
    { name = 8, layout = awful.layout.layouts[1] },
    { name = 9, layout = awful.layout.layouts[1] },
    { name = "web", layout = awful.layout.layouts[1] },
    { name = "email", layout = awful.layout.layouts[1] },
    { name = "vm", layout = awful.layout.layouts[1] },
})

-- Button mapping for the taglist
local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

awful.screen.connect_for_each_screen(function(s)
    sharedtags.viewonly(tags[s.index], s)
end)

for _, tag in ipairs(tags) do
    tag:connect_signal("tagged", function() mytaglist._do_taglist_update() end)
end

-- Create a taglist widget
mytaglist = awful.widget.taglist {
    screen = primary_screen,
    filter = awful.widget.taglist.filter.noempty,
    buttons = taglist_buttons,
    source = function() return tags end,
    layout = {
        layout = wibox.layout.fixed.horizontal,
        spacing = 1
    },
}
-- }}}

-- {{{ Promptbox widget
mypromptbox = awful.widget.prompt()
-- }}}

-- {{{ Pomodoro
pomo_widget = require("pomo")
-- }}}

-- {{{ Create the wibar
mywibox = awful.wibar {
    position = "top",
    screen = primary_screen,
}

-- Add widgets to the wibar
mywibox:setup {
    layout = wibox.layout.align.horizontal,
    { -- Left widgets
        layout = wibox.layout.fixed.horizontal,
        mytaglist,
        mypromptbox
    },
    wibox.widget.base.empty_widget(),
    { -- Right widgets
        layout = wibox.layout.fixed.horizontal,
        spacing_widget = wibox.widget.separator,
        spacing = 20,
        pomo_widget,
        network_widget,
        brightness_widget,
        battery_widget,
        mode_widget,
        textclock,
    },
}
-- }}}

-- }}}

-- {{{ Global mouse bindings
root.buttons(gears.table.join(
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Keymap management
mapstack = {}

function mapstack:push(map)
    root.keys(map.map)
    mode_widget.markup = map.name
    mapstack[table.maxn(mapstack) + 1] = map
end

function mapstack:pop()
    mapstack[table.maxn(mapstack)] = nil
    root.keys(mapstack[table.maxn(mapstack)].map)
    mode_widget.text = mapstack[table.maxn(mapstack)].name
end

function noop(...) end
-- }}}

-- {{{ Key bindings
keymaps = {
    input = {
        name = "INPT",
        map = gears.table.join(
            -- Enter a mode
            awful.key({ }, manipkey, function() mapstack:push(keymaps.manip) end),

            -- Layout manipulation
            awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
            awful.key({ modkey,           }, "Tab",
                function ()
                    awful.client.focus.history.previous()
                    if client.focus then
                        client.focus:raise()
                    end
                end,
                {description = "go back", group = "client"}),

            -- Standard program
            awful.key({ modkey, "Control" }, "r", awesome.restart),
            awful.key({ modkey, "Shift"   }, "q", awesome.quit),

            awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
                      {description = "increase master width factor", group = "layout"}),
            awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
                      {description = "decrease master width factor", group = "layout"}),
            awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
                      {description = "increase the number of master clients", group = "layout"}),
            awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
                      {description = "decrease the number of master clients", group = "layout"}),
            awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
                      {description = "increase the number of columns", group = "layout"}),
            awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
                      {description = "decrease the number of columns", group = "layout"}),

            -- Menubar
            awful.key({ modkey }, "p", function() menubar.show() end,
                      {description = "show the menubar", group = "launcher"})
        )
    },
    manip = {
        name = "<span background='" .. beautiful.bg_warning .. "' foreground='" .. beautiful.fg_warning .. "'>CTRL</span>",
        map = gears.table.join(
            -- Leave manip mode
            awful.key({ }, "Escape", mapstack.pop),
            awful.key({ }, manipkey, mapstack.pop),

            -- Select window
            awful.key({ }, "j", function() awful.client.focus.byidx(1) end),
            awful.key({ }, "k", function() awful.client.focus.byidx(-1) end),

            -- Select screen
            awful.key({ }, "l", function() awful.screen.focus_relative(1) end),
            awful.key({ }, "h", function() awful.screen.focus_relative(-1) end),

            -- Close window
            awful.key({ }, "c", function()
                if client.focus then
                    client.focus:kill()
                end
            end),

            -- Close all windows in tag
            awful.key({ "Shift" }, "c", function()
                for _, c in pairs(awful.screen.focused().selected_tag:clients()) do
                    c:kill()
                end
            end),

            -- Window positioning
            awful.key({ "Shift" }, "j", function() awful.client.swap.byidx(1) end),
            awful.key({ "Shift" }, "k", function() awful.client.swap.byidx(-1) end),

            -- Prompt to run program
            awful.key({ }, "r", function()
                mypromptbox:run()
                mapstack:pop()
            end),

            -- Spawn terminal (leaving manip mode after)
            awful.key({ }, "Return", function()
                awful.spawn(terminal)
                mapstack:pop()
            end),

            awful.key({ }, "w", function()
                awful.screen.focus(utility_screen)
                sharedtags.viewonly(tags["web"], awful.screen.focused())

                need_to_spawn = true
                for c in awful.client.iterate(function() return true end) do
                    if c.class == "firefoxdeveloperedition" and c.first_tag == tags["web"] then
                        need_to_spawn = false
                        break
                    end
                end

                if need_to_spawn then
                    awful.spawn.spawn("firefox-developer-edition")
                end
            end),

            awful.key({ "Shift" }, "w", function()
                if client.focus then
                    client.focus:move_to_tag(tags["web"])
                end
            end),

            awful.key({ }, "e", function()
                awful.screen.focus(utility_screen)
                sharedtags.viewonly(tags["email"], awful.screen.focused())

                need_to_spawn = true
                for c in awful.client.iterate(function() return true end) do
                    if c.class == "thunderbird" and c.first_tag == tags["email"] then
                        need_to_spawn = false
                        break
                    end
                end

                if need_to_spawn then
                    awful.spawn.spawn("thunderbird")
                end
            end),

            awful.key({ "Shift" }, "e", function()
                if client.focus then
                    client.focus:move_to_tag(tags["email"])
                end
            end),

            awful.key({ }, "v", function()
                sharedtags.viewonly(tags["vm"], awful.screen.focused())
            end),

            awful.key({ "Shift" }, "v", function()
                if client.focus then
                    client.focus:move_to_tag(tags["vm"])
                end
            end),

            -- Toggle layouts
            awful.key({ }, "space", function()
                local t = awful.screen.focused().selected_tag

                local maximize = false

                if client.focus then
                    maximize = not client.focus.maximized
                    client.focus:raise()
                end

                for _, c in ipairs(t:clients()) do
                    c.maximized = maximize
                end

            end),
            awful.key({ "Shift" }, "space", function() awful.layout.inc(-1) end),

            -- Restart and quit Awesome itself
            awful.key({ }, "q", awesome.restart),
            awful.key({ "Shift" }, "q", awesome.quit)
        )
    }
}

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    keymaps.manip.map = gears.table.join(keymaps.manip.map,
        -- View tag only.
        awful.key({ }, "#" .. i + 9,
            function ()
                local screen = awful.screen.focused()
                local tag = tags[i]
                if tag then
                   sharedtags.viewonly(tag, screen)
                end

                -- Update taglist
                -- TODO: When (if?) this gets stabilized, change to it
                mytaglist._do_taglist_update()
            end),
        -- Move client to tag.
        awful.key({ "Shift" }, "#" .. i + 9,
            function ()
                if client.focus then
                    local tag = tags[i]

                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end)
    )
end

mapstack:push(keymaps.input)

clientkeys = gears.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "(un)maximize", group = "client"}),
    awful.key({ modkey, "Control" }, "m",
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        {description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey, "Shift"   }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        {description = "(un)maximize horizontally", group = "client"})
)

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
          "pinentry",
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "veromix",
          "xtightvncviewer"},

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "ConfigManager",  -- Thunderbird's about:config.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { 
          floating = true,
          titlebars_enabled = true
      }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "dialog" } }, properties = { titlebars_enabled = true }
    },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c)
    -- Format focused client
    c.border_color = beautiful.border_focus
    c.opacity = 1

    -- Set mouse to middle of screen
    m = mouse.coords()
    geom = c:geometry()

    slop = 2

    if (m.x + slop < geom.x 
        or m.x - slop > geom.x + geom.width
        or m.y + slop < geom.y
        or m.y + slop > geom.y + geom.height) then

        mouse.coords({
            x = geom.x + geom.width/2,
            y = geom.y + geom.height/2,
        }, true)
    end
end)

-- Set borders and opacity on for unfocused windows
client.connect_signal("unfocus", function(c)
    c.border_color = beautiful.border_normal
    c.opacity = 0.9
end)
-- }}}
