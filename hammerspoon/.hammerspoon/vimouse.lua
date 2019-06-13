-- Save to ~/.hammerspoon
-- In ~/.hammerspoon/init.lua:
--    local vimouse = require('vimouse')
--    vimouse('cmd', 'm')
--
-- This sets cmd-m as the key that toggles Vi Mouse.
--
-- h/j/k/l moves the mouse cursor by 20 pixels.  Holding shift moves by 100
-- pixels, and holding alt moves by 5 pixels.
--
-- Pressing <space> sends left mouse down.  Releasing <space> sends left mouse
-- up.  Holding <space> and pressing h/j/k/l is mouse dragging.  Tapping
-- <space> quickly sends double and triple clicks.  Holding alt sends right
-- mouse events.
--
-- <a-j> and <a-k> sends the scroll wheel event.  Holding the keys will speed
-- up the scrolling.
--
-- Press <esc> or the configured toggle key to end Vi Mouse mode.

return function(tmod, tkey)
  -- local overlay = nil
  local log = hs.logger.new('vimouse', 'debug')
  local tap = nil
  local orig_coords = nil
  local dragging = false
  local scrolling = 0
  local mousedown_time = 0
  local mousepress_time = 0
  local mousepress = 0
  local tapmods = {['cmd']=false, ['ctrl']=false, ['alt']=false, ['shift']=false}

  if type(tmod) == 'string' then
    tapmods[tmod] = true
  else
    for _, name in ipairs(tmod) do
      tapmods[name] = true
    end
  end

  local eventTypes = hs.eventtap.event.types
  local eventPropTypes = hs.eventtap.event.properties
  local keycodes = hs.keycodes.map

  function postEvent(et, coords, modkeys, clicks)
    local e = hs.eventtap.event.newMouseEvent(et, coords, modkeys)
    if clicks > 3 then
      clicks = 3
    end
    e:setProperty(eventPropTypes.mouseEventClickState, clicks)
    e:post()
  end

  tap = hs.eventtap.new({eventTypes.keyDown, eventTypes.keyUp}, function(event)
    local code = event:getKeyCode()
    local flags = event:getFlags()
    local repeating = event:getProperty(eventPropTypes.keyboardEventAutorepeat)
    local coords = hs.mouse.getAbsolutePosition()

    if (code == keycodes.tab or code == keycodes['`']) and flags.cmd then
      -- Window cycling
      return false
    end

    if code == keycodes.space then
      -- Mouse clicking
      if repeating ~= 0 then
        return true
      end

      local btn = 'left'
      if flags.ctrl then
        btn = 'right'
      end

      local now = hs.timer.secondsSinceEpoch()
      if now - mousepress_time > hs.eventtap.doubleClickInterval() then
        mousepress = 1
      end

      if event:getType() == eventTypes.keyUp then
        dragging = false
        postEvent(eventTypes[btn..'MouseUp'], coords, flags, mousepress)
      elseif event:getType() == eventTypes.keyDown then
        dragging = true
        if now - mousedown_time <= 0.3 then
          mousepress = mousepress + 1
          mousepress_time = now
        end

        mousedown_time = hs.timer.secondsSinceEpoch()
        postEvent(eventTypes[btn..'MouseDown'], coords, flags, mousepress)
      end

      orig_coords = coords
    elseif event:getType() == eventTypes.keyDown then
      local mul = 0
      local step = 10
      local x_delta = 0
      local y_delta = 0
      local scroll_y_delta = 0
      local is_tapkey = code == keycodes[tkey]

      if is_tapkey == true then
        for name, _ in pairs(tapmods) do
          if flags[name] == nil then
            flags[name] = false
          end

          if tapmods[name] ~= flags[name] then
            is_tapkey = false
            break
          end
        end
      end

      if flags.alt then
        step = 5
      end

      if flags.shift then
        mul = 5
      else
        mul = 1
      end

      if is_tapkey or code == keycodes['escape'] then
        if dragging then
          postEvent(eventTypes.leftMouseUp, coords, flags, 0)
        end
        dragging = false
        -- overlay:delete()
        -- overlay = nil
        hs.alert('Vi Mouse Off')
        tap:stop()
        hs.mouse.setAbsolutePosition(orig_coords)
        return true
      elseif (code == keycodes['j'] or code == keycodes['k']) and flags.alt then
        if repeating ~= 0 then
          scrolling = scrolling + 1
        else
          scrolling = 1
        end

        local scroll_mul = 1 + math.log(scrolling)
        if code == keycodes['j'] then
          scroll_y_delta = math.ceil(-1 * scroll_mul)
        else
          scroll_y_delta = math.floor(1 * scroll_mul)
        end
        log.d("Scrolling", scrolling, '-', scroll_y_delta)
      elseif code == keycodes['h'] then
        x_delta = step * mul * -1
      elseif code == keycodes['l'] then
        x_delta = step * mul
      elseif code == keycodes['j'] then
        y_delta = step * mul
      elseif code == keycodes['k'] then
        y_delta = step * mul * -1
      end

      if scroll_y_delta ~= 0 then
        hs.eventtap.event.newScrollEvent({0, scroll_y_delta}, flags, 'line'):post()
      end

      if x_delta or y_delta then
        coords.x = coords.x + x_delta
        coords.y = coords.y + y_delta

        if dragging then
          postEvent(eventTypes.leftMouseDragged, coords, flags, 0)
        else
          hs.mouse.setAbsolutePosition(coords)
        end
      end
    end
    return true
  end)

  hs.hotkey.bind(tmod, tkey, nil, function(event)
    local screen = hs.mouse.getCurrentScreen()
    local frame = screen:fullFrame()

    -- overlay = hs.drawing.rectangle(frame)
    -- overlay:setFillColor({['red']=0, ['blue']=0, ['green']=0, ['alpha']=0.2})
    -- overlay:setFill(true)
    -- overlay:setLevel(hs.drawing.windowLevels['assistiveTechHigh'])
    -- overlay:show()

    hs.alert('Vi Mouse On')
    orig_coords = hs.mouse.getAbsolutePosition()
    tap:start()
  end)
end
