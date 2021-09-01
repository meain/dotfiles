function conky(id, contentFunction, position, color, time, fontFamily, fontSize)
    local canvas = hs.canvas.new(position)
    canvas:behavior(hs.canvas.windowBehaviors.canJoinAllSpaces)
    canvas:level(hs.canvas.windowLevels.desktopIcon)

    canvas[1] = {
        id = id,
        type = "text",
        text = contentFunction(),
        textFont = fontFamily,
        textSize = fontSize,
        textColor = color,
        textAlignment = "left"
    }
    canvas:show()
    hs.timer.doEvery(
        time,
        function()
            canvas[1].text = contentFunction()
            canvas:show()
        end
    )
end

return conky
