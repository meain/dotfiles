local keys = {}

keys.fkey = {"alt", "ctrl", "cmd"}
keys.hyper = {"alt", "ctrl", "cmd", "shift"}

-- alert styling
hs.alert.defaultStyle.strokeWidth = 0
hs.alert.defaultStyle.radius = 0
hs.alert.defaultStyle.textFont = "Monaco"
hs.alert.defaultStyle.textSize = 13
hs.alert.defaultStyle.fadeInDuration = 0.10
hs.alert.defaultStyle.fadeOutDuration = 1
hs.alert.defaultStyle.atScreenEdge = 2 -- need multiple items
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.95 }

return keys
