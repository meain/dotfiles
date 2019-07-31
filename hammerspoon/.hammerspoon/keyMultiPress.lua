local alert    = require("hs.alert")
local timer    = require("hs.timer")
local eventtap = require("hs.eventtap")

local events   = eventtap.event.types

local module   = {}

module.timeFrame = 1  -- how quickly must the two single ctrl taps occur?
module.clickCount = 2  -- how many clicks
module.keyType = 'shift'  -- which key

-- what to do when the double tap of ctrl occurs
module.action = function()
    alert("You tapped " .. module.keyType .. " " .. module.clickCount .. " times!")
end


local timeFirstControl, downCount = 0, 0

-- verify that no keyboard flags are being pressed
local noFlags = function(ev)
    local result = true
    for k,v in pairs(ev:getFlags()) do
        if v then
            result = false
            break
        end
    end
    return result
end

-- verify that *only* the specified key is being pressed
local onlySpecifiedKey = function(ev)
    local result = ev:getFlags().shift
    for k,v in pairs(ev:getFlags()) do
        if k ~= module.keyType and v then
            result = false
            break
        end
    end
    return result
end

-- the actual workhorse
module.eventWatcher = eventtap.new({events.flagsChanged, events.keyDown}, function(ev)
    -- if it's been too long; previous state doesn't matter
    if (timer.secondsSinceEpoch() - timeFirstControl) > module.timeFrame then
        timeFirstControl, clickCount = 0, 0
    end

    if ev:getType() == events.flagsChanged then
        if noFlags(ev) and downCount == module.clickCount then  -- ctrl up and we've seen two, so do action
            if module.action then module.action() end
            timeFirstControl, downCount = 0, 0
        elseif onlySpecifiedKey(ev) and downCount == 0 then -- ctrl down and it's a first
            downCount = downCount + 1
            timeFirstControl = timer.secondsSinceEpoch()
        elseif onlySpecifiedKey(ev) and downCount > 0 then  -- ctrl down and it's the second
            downCount = downCount + 1
        elseif not noFlags(ev) then  -- otherwise reset and start over
            timeFirstControl, downCount = 0, 0
        end
    else  -- it was a key press, so not a lone ctrl char -- we don't care about it
        timeFirstControl, downCount = 0, 0
    end
    return false
end):start()

return module
