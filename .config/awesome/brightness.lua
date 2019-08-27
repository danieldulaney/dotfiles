-- vim: set foldmethod=marker :

-- Future me:
-- This one is easy to screw up and make an infinite loop. In particular:
--  1. User changes the slider value
--  2. Call light -S to set the brightness
--  3. Update the real-world brightness
--  4. Check the real-world brightness
--  5. Set the slider value
--  6. Goto 1
-- Currently, #3 sets the brightness text, but not the slider. #4 isn't called
-- immediately, only on a timer. However, if either #3 calls #4 directly, or
-- #3 sets the value, thus calling #5, then it's possible to end up with slider
-- values jumping back and forth quickly.
--
-- Setting the text immediately on changing the value but leaving the slider
-- where it is until the next 1-second query loop is a good compromise. Ideally,
-- we'd replace the query loop with some kind of watcher.

-- {{{ Requires
local awful = require("awful")
local naughty = require("naughty")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")
-- }}}

-- {{{ Widget definitions
local brightness_slider = wibox.widget {
        forced_width = 100,
        widget = wibox.widget.slider,
        minimum = 1,
        maximum = 100,
        visible = false,
        active = false,
}

local brightness_text = wibox.widget.textbox("")

local brightness_widget = wibox.widget {
    layout = wibox.layout.fixed.horizontal,
    spacing = 0,

    brightness_slider,
    brightness_text,
}
-- }}}

-- {{{ Brightness I/O helpers

-- Set the text while leaving the slider unchanged
function set_brightness_text(value)
    if value == nil then
        brightness_text.markup = ""
    elseif value == 100 then
        brightness_text.markup = " 100"
    else
        brightness_text.markup = string.format(" %2d%%", value)
    end

end

-- Set the brightness. If there's already one running, don't bother
local set_brightness_running = false
function set_brightness(value)

    if not set_brightness_running then
        set_brightness_running = true

        awful.spawn.easy_async("light -S " .. tostring(value), function(stdout, stderr, exitreason, exitcode)
            set_brightness_running = false
            set_brightness_text(value)
        end)
    end
end

function check_brightness_async()
    awful.spawn.easy_async("light -G", function(stdout, stderr, exitreason, exitcode)
        if stderr:len() > 0 then
            set_brightness_text(nil)

            brightness_slider.active = false
        else
            local brightness = tonumber(stdout)

            set_brightness_text(brightness)

            brightness_slider.active = true 
            brightness_slider.value = brightness
        end
    end)
end
-- }}}

-- {{{ External event callbacks
function set_slider_visibility(visible)

    if brightness_slider.active or not visible then

        brightness_slider.visible = visible

        if visible then
            brightness_widget.spacing = 10
        else
            brightness_widget.spacing = 0
        end

    end
end

brightness_slider:connect_signal("property::value", function(widget)
    set_brightness(widget.value)
end)

brightness_widget:connect_signal("mouse::leave", function()
    set_slider_visibility(false)
end)

brightness_text:buttons(
    awful.button({ }, 1, function()
        set_slider_visibility(not brightness_slider.visible)
    end)
)
-- }}}

-- Checking loop that should be replaced with change notification if possible
gears.timer {
    timeout = 2,
    call_now = true,
    autostart = true,
    callback = check_brightness_async,
}

return brightness_widget
