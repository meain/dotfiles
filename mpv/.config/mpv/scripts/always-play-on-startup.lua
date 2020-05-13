-- This script will cause mpv to always play on startup (since pause=no doesn't
-- seem to work in mpv.conf)

mp.register_event(
    "file-loaded", function() mp.set_property_bool("pause", false) end
)
