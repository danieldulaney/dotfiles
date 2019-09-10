-- vim: set foldmethod=marker :

-- {{{ Requires
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local naughty = require("naughty")
-- }}}

-- {{{ Widget state
local network_widget = wibox.widget.textbox("")

network_widget.tip = "Fetching status..."
network_widget._icon1 = ""
network_widget._icon2 = ""

network_widget.update_markup = function(self)
    self.markup = self._icon1 .. " " .. self._icon2
end

network_widget.set_icon1 = function(self, icon)
    self._icon1 = icon
    self:update_markup()
end

network_widget.set_icon2 = function(self, icon)
    self._icon2 = icon
    self:update_markup()
end

local has_iwctl = nil
local has_networkctl = nil
local has_curl = nil
-- }}}

-- {{{ Tooltip
awful.tooltip {
    objects = { network_widget },
    timer_function = function()
        return network_widget.tip
    end,
    mode = "outside",
    preferred_positions = {"bottom"},
}
-- }}}

-- {{{ I/O helpers
function check_availables()
    awful.spawn.easy_async("which iwctl", function(_, _, _, exitcode)
        has_iwctl = exitcode == 0
    end)

    awful.spawn.easy_async("which networkctl", function(_, _, _, exitcode)
        has_networkctl = exitcode == 0
        update_local()
    end)

    awful.spawn.easy_async("which curl", function(_, _, _, exitcode)
        has_curl = exitcode == 0
        update_remote()
    end)
end

function update_local()
    if not has_networkctl then
        naughty.notify{ text = "No networkctl" }
        return
    end

    awful.spawn.easy_async("networkctl status", function(stdout, _, _, _)
        network_widget.tip = full_status_from_raw(stdout)
        network_widget:set_icon1(icon1_from_raw(stdout))
    end)
end

function update_remote()
    if not has_curl then
        naughty.notify{ text = "No curl" }
        return
    end

    awful.spawn.easy_async("curl --head example.com", function(_, _, _, exitcode)
        if exitcode == 0 then
            network_widget:set_icon2("")
        else
            network_widget:set_icon2("")
        end
    end)
end

function trigger_scan()
    if not has_iwctl then
        naughty.notify{ text = "No iwctl" }
        return
    end

    awful.spawn.easy_async("", function(_, _, _, exitcode)
    end)
end
-- }}}

-- {{{ Parsers etc.
function parse_raw_status(raw_status)
    local results = {}

    for line in (raw_status..'\n'):gmatch("(.-)\r?\n") do
        for key, value in line:gmatch("([%w]+): (.+)") do

            if key == "Address" or key == "Gateway" then
                for data, interface in value:gmatch("(.*) on (%w+)") do
                    results[key] = data
                    results["Interface"] = interface
                end
            else
                results[key] = value
            end
        end
    end

    return results
end

function full_status_from_raw(raw_status)
    local trimmed = raw_status:match('^(.*%S)') or ''

    return trimmed
end

function icon1_from_raw(raw_status)
    local table = parse_raw_status(raw_status)

    local icon1

    if table["State"] == "no-carrier" or not table["Interface"] then
        icon1 = ""
    elseif string.sub(table["Interface"], 1, 1) == "w" then
        icon1 = ""
    elseif string.sub(table["Interface"], 1, 1) == "e" then
        icon1 = ""
    else
        icon1 = ""
    end

    return icon1
end
-- }}}

-- {{{ Buttons
network_widget:buttons(
    awful.button({ }, 1, trigger_scan)
)
-- }}}

check_availables()
network_widget:update_markup()

-- Checking loop that should be replaced with change notification if possible
gears.timer {
    timeout = 2,
    call_now = false,
    autostart = true,
    callback = update_local,
}

gears.timer {
    timeout = 10,
    call_now = false,
    autostart = true,
    callback = update_remote
}

return network_widget
