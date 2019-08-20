-- Find my mouse pointer

local mouseCircle = nil
local mouseCircleTimer = nil

function mouseHighlight()
    -- Delete an existing highlight if it exists
    if mouseCircle then
        mouseCircle:delete()
        if mouseCircleTimer then
            mouseCircleTimer:stop()
        end
    end
    -- Get the current co-ordinates of the mouse pointer
    mousepoint = hs.mouse.getAbsolutePosition ()
    -- Prepare a big red circle around the mouse pointer
    mouseCircle = hs.drawing.circle(hs.geometry.rect(mousepoint.x-4000, mousepoint.y-4000, 8000, 8000))
    mouseCircle:setStrokeColor({["red"]=0.25,["blue"]=0.69,["green"]=0.95,["alpha"]=0.5})
    mouseCircle:setFill(false)
    mouseCircle:setStrokeWidth(3980)
    mouseCircle:show()

    mouseCircleTimer = hs.timer.doAfter(1, function() mouseCircle:delete() end)
end

hs.hotkey.bind({"alt","shift"}, "M", mouseHighlight)
