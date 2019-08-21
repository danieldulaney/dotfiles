local awful = require("awful")
local naughty = require("naughty")
local watch = require("awful.widget.watch")
local wibox = require("wibox")
local beautiful = require("beautiful")

local battery_widget = wibox.widget.textbox()

battery_widget.markup = "reading battery..."
battery_widget.tip = "reading battery..."

awful.tooltip {
    objects = { battery_widget },
    timer_function = function()
        return battery_widget.tip
    end,
    mode = "outside",
    preferred_positions = {"bottom"},
}

watch("acpi -ab", 1,
    function(widget, stdout, stderr, exitreason, exitcode)

        -- Fill these in throughout the loop
        local ac_on = nil
        local batt_line = nil
        local batt_status = nil
        local batt_level = nil

        for line in stdout:gmatch("[^\r\n]+") do

            local name, kind, state = string.match(
                line,
                "(([^:]*) %d+): (.*)"
            )

            if kind == "Adapter" then
                if state == "on-line" then
                    ac_on = true
                elseif state == "off-line" then
                    ac_on = false
                end
            elseif kind == "Battery" then
                local status, pct = string.match(
                    state,
                    "(%w+), (%d+)%%"
                )

                batt_line = state
                batt_status = status
                batt_level = tonumber(pct) / 100.0
            end
        end

        -- Assemble the icons
        local icon = ""
        local text = "reading battery..."
        local pct_str = ""
        local bg = beautiful.bg_normal
        local fg = beautiful.fg_normal

        if ac_on then
            icon = ""
        elseif batt_status == "Discharging" then
            if batt_level > 0.875 then
                icon = ""
            elseif batt_level > 0.625 then
                icon = ""
            elseif batt_level > 0.375 then
                icon = ""
            elseif batt_level > 0.125 then
                icon = ""
                bg = beautiful.bg_warning
                fg = beautiful.fg_warning
            else
                icon = ""
                bg = beautiful.bg_urgent
                fg = beautiful.fg_urgent
            end
        end

        if batt_level ~= nil and 0 <= batt_level and batt_level < 1 then
            text = string.format(" %2.0f%%", batt_level * 100)
        elseif batt_level == 1 then
            text = " 100"
        else
            text = ""
        end

        battery_widget.markup =
            "<span background='"
            .. bg
            .. "' foreground='"
            .. fg
            .. "'>" .. icon .. "</span>"
            .. text

        battery_widget.tip = batt_line
    end
)

return battery_widget
