local obj = {}

local chooser = require("hs.chooser")
local dialog = require("hs.dialog")
local styledtext = require("hs.styledtext")

local customshellrun = require("customshellrun")
local utils = require("utils")

-- print(styledtext.fontNames())

completionFn = function(choice)
    if choice == nil then
        return
    end
    if choice["id"] == "new" then
        hs.focus() -- this is needed for the textPrompt to have focus
        local button, newTask =
            dialog.textPrompt("New task", "Add new task entry", "<project> task description", "Add", "Cancel")
        if button == "Add" then
            local result = customshellrun.run("sp " .. newTask, true)
            hs.alert("➕ Added new task: +" .. newTask)
            hs.alert(result)
        end
    else
        hs.alert("✔ " .. choice["project"] .. ": " .. choice["task"]:gsub("%s+", "", 1))
        local result = customshellrun.run("/usr/local/bin/task " .. choice["id"] .. " done")
    end
end

function obj.run()
    local selecter = chooser.new(completionFn)
    selecter:placeholderText("Choose task to mark complete")

    local result = customshellrun.run("/usr/local/bin/task totn")
    local tasks = utils.split(result, "\n")

    local choices = {}

    for i = #tasks - 1, 3, -1 do
        local words = {}
        words[1], words[2], words[3] = tasks[i]:match(" ?(%d-) (%w+)(.+)")

        local text =
            styledtext.new(
            words[3]:gsub("%s+", "", 1),
            {
                font = {size = 17, name = "DankMonoNerdFontComplete-Regular"}
            }
        )
        local subtext =
            styledtext.new(
            words[1] .. " " .. words[2],
            {
                font = {size = 12, name = "DankMonoNerdFontComplete-Regular"},
                paragraphStyle = {
                    paragraphSpacing = 10.0
                }
            }
        )
        table.insert(
            choices,
            1,
            {["text"] = text, ["subText"] = subtext, ["id"] = words[1], ["task"] = words[3], ["project"] = words[2]}
        )
    end
    table.insert(
        choices,
        1,
        {
            ["text"] = styledtext.new(
                "Create new task",
                {
                    font = {size = 17, name = "DankMonoNerdFontComplete-Regular"}
                }
            ),
            ["subText"] = "",
            ["id"] = "new",
            ["task"] = "",
            ["project"] = ""
        }
    )
    selecter:choices(choices)
    selecter:searchSubText(true)
    selecter:show()
end

return obj
