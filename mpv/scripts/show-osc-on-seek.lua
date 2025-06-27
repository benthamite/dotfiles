local mp = require("mp")

local function format_time_ms(seconds)
    if not seconds or seconds < 0 then
        return "00:00.000"
    end
    
    local hours = math.floor(seconds / 3600)
    local mins = math.floor((seconds % 3600) / 60)
    local secs = seconds % 60
    local whole_secs = math.floor(secs)
    local ms = math.floor((secs - whole_secs) * 1000)
    
    if hours > 0 then
        return string.format("%d:%02d:%02d.%03d", hours, mins, whole_secs, ms)
    else
        return string.format("%02d:%02d.%03d", mins, whole_secs, ms)
    end
end

mp.register_event("seek", function()
    local time_pos = mp.get_property_number("playback-time")
    local duration = mp.get_property_number("duration")
    local percent = mp.get_property_number("percent-pos")
    
    local time_str = format_time_ms(time_pos)
    local duration_str = format_time_ms(duration)
    
    mp.osd_message(string.format("%s / %s (%d%%)", 
        time_str, duration_str, math.floor(percent or 0)), 2)
end)
