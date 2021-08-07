local utils = require("utils")
local customshellrun = require("customshellrun")

function workspaceLogin(filename)
    local url = customshellrun.run("pass show workspaces/url", true)
    local pin = customshellrun.run("pass show workspaces/pin", true)
    local username = customshellrun.run("pass show workspaces/username", true)
    local password = customshellrun.run("pass show workspaces/password", true)
    customshellrun.run("open " .. url, true)
    hs.application.launchOrFocus("workspaces")
    os.execute("sleep " .. tonumber(15))
    hs.application.launchOrFocus("firefox")
    hs.eventtap.keyStrokes(pin)
    hs.eventtap.keyStroke({}, "return")
    hs.application.launchOrFocus("workspaces")
    hs.eventtap.keyStroke({}, "tab")
    hs.eventtap.keyStrokes(username)
    hs.eventtap.keyStroke({}, "tab")
    hs.eventtap.keyStrokes(password)
    hs.eventtap.keyStroke({}, "tab")
    hs.application.launchOrFocus("firefox")
    local clipChanged = utils.waitTillClipChanges(10)
    if clipChanged then
        hs.application.launchOrFocus("workspaces")
        hs.eventtap.keyStroke({"cmd"}, "v")
        hs.eventtap.keyStroke({}, "return")
        hs.alert("Login complete")
    end
end

return workspaceLogin

