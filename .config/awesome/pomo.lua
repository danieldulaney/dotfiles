local naughty = require("naughty") local wibox = require("wibox")
local os = require("os")
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")

local timers = {
    {
        time = 25 * 60,
        name = "W",
    },
    {
        time = 5 * 60,
        name = "B",
    },
}

-- {{{ Utility functions
function round(num)
    return math.floor(num + 0.5)
end

function sec_to_time(secs)
    if secs == nil then
        return "????"
    elseif secs > 60 then
        return string.format("%2d", round(secs/60))
    elseif secs > 0 then
        return "&lt;1"
    else
        return " 0"
    end
end
-- }}}

local pomo_widget = wibox.widget.textbox("hi there")

-- Widget state
pomo_widget.curr_timer = 1
pomo_widget.timer_start = nil
pomo_widget.timer_notification = nil

pomo_widget.timer_name = function(self)
    return timers[self.curr_timer].name
end

pomo_widget.timer_len = function(self)
    return timers[self.curr_timer].time
end

pomo_widget.timer_left = function(self, now)
    if self.timer_start == nil then
        return self:timer_len()
    else
        return self:timer_len() + self.timer_start - now
    end
end

pomo_widget.timer_finished = function(self, now)
    if self.timer_start == nil then
        return false
    else
        return now > self.timer_start + self:timer_len()
    end
end

pomo_widget.timer_icon = function(self, now)
    if self.timer_start == nil then
        return ""
    elseif self:timer_finished(now) then
        return ""
    else
        return ""
    end
end

pomo_widget.update = function(self)
    local now = os.time()

    local fg
    local bg

    if self:timer_finished(now) then
        fg = beautiful.fg_warning
        bg = beautiful.bg_warning
    else
        fg = beautiful.fg_normal
        bg = beautiful.bg_normal
    end

    self.markup = string.format("<span foreground='%s' background='%s'>%s %s %s</span>",
        fg,
        bg,
        self:timer_icon(now),
        self:timer_name(),
        sec_to_time(self:timer_left(now))
    )

    if self:timer_finished(now) and self.timer_notification == nil then
        self.timer_notification = naughty.notify {
            text = "Timer finished",
            position = "top_middle",
            timeout = 0,
            fg = beautiful.fg_warning,
            bg = beautiful.bg_warning,
            destroy = function() pomo_widget:cancel() end,
        }
    end
end

pomo_widget.start = function(self, restart)
    if restart or self.timer_start == nil then
        self.timer_start = os.time()
    end

    self:update()
end

pomo_widget.cancel = function(self)
    if self.timer_notification ~= nil then
        naughty.destroy(self.timer_notification)
    end

    self.timer_start = nil
    self.timer_alerted = false
    self.timer_notification = nil
    self:update()
end

pomo_widget.next = function(self)
    now = os.time()

    if self:timer_finished(now) or self.timer_start == nil then
        self:cancel()

        self.curr_timer = self.curr_timer + 1

        if self.curr_timer > #timers then
            self.curr_timer = 1
        end
    end

    self:update()
end

pomo_widget:buttons(
    gears.table.join(
        awful.button({ }, 1, function() pomo_widget:start(false) end),
        awful.button({ }, 2, function() pomo_widget:cancel(true) end),
        awful.button({ }, 3, function() pomo_widget:next() end)
    )
)

gears.timer {
    timeout = 1,
    call_now = true,
    autostart = true,
    callback = function() pomo_widget:update() end,
}

return pomo_widget
