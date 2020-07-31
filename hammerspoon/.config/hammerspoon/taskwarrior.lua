local obj = {}

local chooser = require("hs.chooser")
local styledtext = require("hs.styledtext")

local customshellrun = require("customshellrun")
local utils = require("utils")

print(styledtext.fontNames())

completionFn = function(choice)
    print(choice["id"])
    hs.alert("✔ " .. choice["project"] .. ": " .. choice["task"]:gsub("%s+", "", 1))
    local result = customshellrun.run("/usr/local/bin/task " .. choice["id"] .. " done")
end

function obj.run()
    local selecter = chooser.new(completionFn)
    selecter:placeholderText("Choose task to mark complete")

    local result = customshellrun.run("/usr/local/bin/task totn")
    local tasks = utils.split(result, "\n")
    print(result)

    local choices = {}

    for i = #tasks - 1, 3, -1 do
        local words = {}
        words[1], words[2], words[3] = tasks[i]:match(" ?(%d-) (%w+)(.+)")
        print(words[2])

        local text =
            styledtext.new(
            words[3]:gsub("%s+", "", 1),
            {
                font = {size = 17}
            }
        )
        local subtext =
            styledtext.new(
            words[1] .. " " .. words[2],
            {
                font = {size = 13}
            }
        )
        table.insert(
            choices,
            1,
            {["text"] = text, ["subText"] = subtext, ["id"] = words[1], ["task"] = words[3], ["project"] = words[2]}
        )
    end
    print(choices)
    selecter:choices(choices)
    selecter:searchSubText(true)
    selecter:show()
end

return obj
