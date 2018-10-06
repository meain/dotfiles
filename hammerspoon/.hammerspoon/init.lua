-- Set up logger
local logLevel = 'info'
local log = hs.logger.new('hms', logLevel)


-- Disable animations
hs.window.animationDuration = 0


-- Simle key remaps

-- Auto-reload config on change
function reloadConfig(files)
  doReload = false
  for _,file in pairs(files) do
    if file:sub(-4) == ".lua" then
      doReload = true
    end
  end
  if doReload then
    hs.reload()
  end
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.notify.new({title="Hammerspoon", informativeText="Hammerspoon config reloaded!"}):send()
