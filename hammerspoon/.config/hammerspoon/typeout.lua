local utils = require("utils")
function typeout(data)
    hs.alert("⌨ " .. utils.trim(data))
    hs.eventtap.keyStrokes(data)
end

return typeout
