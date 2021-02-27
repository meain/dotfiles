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
        hs.focus() -- this is needed for the textPrompt to have focus
        local button, message =
            dialog.textPrompt("Process task", choice["task"]:gsub("%s+", "", 1), "done", "Run", "Cancel")
        if button == "Run" then
            hs.alert("✔ " .. choice["project"] .. ": " .. choice["task"]:gsub("%s+", "", 1))
            if message == "open" then
                customshellrun.run(
                    "/usr/local/bin/task " ..
                        choice["id"] .. " | grep -Eo 'https?://[a-zA-Z0-9./?=_%:-]*' | sort -u | xargs -L 1 open"
                )
            else
                customshellrun.run("/usr/local/bin/task " .. choice["id"] .. " " .. message)
            end
        end
    end
    -- customshellrun.run("/usr/local/bin/task sync &")
end

function obj.run()
    local selecter = chooser.new(completionFn)
    selecter:width(69)
    selecter:placeholderText("Choose task to mark complete")

    local result = customshellrun.run("/usr/local/bin/task totn")
    local tasks = utils.split(result, "\n")

    local choices = {}

    for i = #tasks - 1, 3, -1 do
        local words = {}
        words[1], words[2], words[3] = tasks[i]:match(" ?(%d-) ([^ ]+)(.+)")

        local text =
            styledtext.new(
            words[2] .. "> " .. words[3]:gsub("%s+", "", 1),
            {
                font = {size = 16, name = "DankMonoNerdFontComplete-Regular"}
            }
        )
        table.insert(choices, 1, {["text"] = text, ["id"] = words[1], ["task"] = words[3], ["project"] = words[2]})
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
