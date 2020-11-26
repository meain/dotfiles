local mod = {}
mod.timeinteral = 1

function switchTime()
    if mod.timeinteral == 1 then
        mod.timeinteral = 15
    elseif mod.timeinteral == 15 then
        mod.timeinteral = 30
    elseif mod.timeinteral == 30 then
        mod.timeinteral = 60
    elseif mod.timeinteral == 60 then
        mod.timeinteral = 1
    end
    print(mod.timeinteral)
    resetTime()
end

function getTimeString(time)
    local minutes = tostring(math.floor(time / 60))
    local seconds = tostring(time % 60)
    if minutes:len() < 2 then
        minutes = "0" .. minutes
    end
    if seconds:len() < 2 then
        seconds = "0" .. seconds
    end
    return minutes .. ":" .. seconds
end

function refreshTime()
    if mod.time > 0 then
        mod.time = mod.time - 1
        mod.menu:setTitle(getTimeString(mod.time))
    else
        hs.notify.new({title = "Timer complete", informativeText = "Time to switch up"}):send()
        resetTime()
    end
end

function resetTime()
    if mod.timer then
        mod.timer:stop()
        mod.timer = nil
    end
    mod.time = mod.timeinteral * 60
    mod.menu:setTitle(getTimeString(mod.time))
end

function clickCallback(clickTable)
    if clickTable.alt then
        resetTime()
        return
    end
    if clickTable.shift then
        switchTime()
        return
    end
    if mod.timer then
        if mod.timer:running() then
            mod.timer:stop()
        else
            mod.timer:start()
        end
        return
    end

    resetTime()
    hs.alert("Starting timer for " .. math.ceil(mod.time / 60) .. "m")
    mod.timer = hs.timer.doEvery(1, refreshTime)
end

function mod.setup(menu)
    mod.menu = menu
    resetTime()
    menu:setTitle(getTimeString(mod.time))
    menu:setClickCallback(clickCallback)
end

return mod
