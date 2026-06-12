-- gotoview.lua: webview-based bookmark launcher
-- To add/edit items, update the `groups` table below.

local customshellrun = require("customshellrun")

local groups = {
    { title = "Work", items = {
        { key = "iw",   label = "workday" },
        { key = "it",   label = "timesheet" },
        { key = "ia",   label = "approvals" },
        { key = "ic",   label = "calendar" },
    }},
    { title = "Jira", items = {
        { key = "bk",   label = "backlog" },
        { key = "dp",   label = "data-pipelines" },
        { key = "cp",   label = "control-plane" },
    }},
    { title = "GitHub", items = {
        { key = "gg",   label = "github" },
        { key = "gn",   label = "notifications" },
        { key = "cpb",  label = "cp-backend" },
        { key = "cpp",  label = "cp-platform" },
    }},
    { title = "Tools", items = {
        { key = "amp",   label = "amplitude" },
        { key = "sp",    label = "speed dial" },
        { key = "inca",  label = "dp alerts" },
        { key = "incdp", label = "dp schedule" },
    }},
    { title = "Dashboard", hint = "d-<env>-<region>", items = {
        { key = "dd",   label = "dev" },
        { key = "dsa",  label = "stage-apj" },
        { key = "dpu",  label = "prod-us" },
    }},
    { title = "Elastic", hint = "e-<env>-<region>", items = {
        { key = "ed",   label = "dev" },
        { key = "esa",  label = "stage-apj" },
        { key = "epu",  label = "prod-us" },
    }},
}

local cols = 3
local w = 540

local function htmlescape(s)
    return s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;")
end

local function buildGroupsHtml()
    local parts = {}
    for _, g in ipairs(groups) do
        table.insert(parts, '<div class="group">')
        table.insert(parts, '<div class="group-title">' .. htmlescape(g.title) .. '</div>')
        for _, item in ipairs(g.items) do
            local cls = item.hint and 'item hint' or 'item'
            table.insert(parts, '<div class="' .. cls .. '"><kbd>' .. htmlescape(item.key) .. '</kbd> ' .. htmlescape(item.label) .. '</div>')
        end
        if g.hint then
            table.insert(parts, '<div class="group-hint">' .. htmlescape(g.hint) .. '</div>')
        end
        table.insert(parts, '</div>')
    end
    return table.concat(parts, '\n')
end

local function show()
    local screen = hs.screen.mainScreen():frame()
    -- start with a generous height; JS will resize after render
    local h = 400
    local x = screen.x + (screen.w - w) / 2
    local y = screen.y + (screen.h - h) / 2

    local wv
    local usercontent = hs.webview.usercontent.new("gotoview")
    usercontent:setCallback(function(msg)
        local body = msg.body
        if body:sub(1, 7) == "resize:" then
            local newH = tonumber(body:sub(8))
            if newH and wv then
                local f = wv:frame()
                local newY = screen.y + (screen.h - newH) / 2
                wv:frame({x=f.x, y=newY, w=f.w, h=newH})
            end
        else
            local s = body
            wv:delete()
            wv = nil
            if s and s ~= "" then
                local result = customshellrun.run(",bm go " .. s)
                if #result > 0 then hs.alert(result) end
            end
        end
    end)

    local html = [[<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<style>
  * { box-sizing: border-box; margin: 0; padding: 0; }
  @font-face {
    font-family: "Victor Mono";
    src: url("file:///Users/meain/Library/Fonts/HomeManager/opentype/VictorMono-Regular.otf");
    font-weight: 400;
  }
  @font-face {
    font-family: "Victor Mono";
    src: url("file:///Users/meain/Library/Fonts/HomeManager/opentype/VictorMono-Medium.otf");
    font-weight: 500;
  }
  @font-face {
    font-family: "Victor Mono";
    src: url("file:///Users/meain/Library/Fonts/HomeManager/opentype/VictorMono-Bold.otf");
    font-weight: 700;
  }
  body {
    font-family: "Victor Mono", "SF Mono", monospace;
    background: #ffffff;
    color: #1c1c1e;
    padding: 16px;
    display: flex;
    flex-direction: column;
    gap: 12px;
    overflow: hidden;
  }
  .groups {
    display: grid;
    grid-template-columns: repeat(]] .. cols .. [[, 1fr);
    gap: 8px;
  }
  .group {
    background: #f2f2f7;
    border-radius: 8px;
    padding: 9px 11px;
  }
  .group-title {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    color: #8e8e93;
    margin-bottom: 5px;
  }
  .item {
    font-size: 12px;
    line-height: 1.7;
    color: #1c1c1e;
  }
  .item kbd {
    display: inline-block;
    background: #e5e5ea;
    color: #007aff;
    border-radius: 4px;
    padding: 0 5px;
    font-family: "Victor Mono", "SF Mono", monospace;
    font-size: 11px;
  }
  .hint { color: #8e8e93; font-size: 11px; }
  .hint kbd { color: #8e8e93; background: #e5e5ea; }
  .group-hint { color: #8e8e93; font-size: 10px; margin-top: 4px; font-family: "Victor Mono", "SF Mono", monospace; }
  .input-row { display: flex; gap: 8px; }
  input {
    flex: 1;
    background: #f2f2f7;
    border: 1px solid #d1d1d6;
    border-radius: 6px;
    color: #1c1c1e;
    font-size: 13px;
    font-family: "Victor Mono", "SF Mono", monospace;
    padding: 7px 10px;
    outline: none;
  }
  input:focus { border-color: #007aff; }
  button {
    background: #007aff;
    color: #ffffff;
    border: none;
    border-radius: 6px;
    font-size: 13px;
    font-weight: 600;
    padding: 7px 16px;
    cursor: pointer;
  }
  button:hover { background: #0051d5; }
</style>
</head>
<body>
<div class="groups">
]] .. buildGroupsHtml() .. [[
</div>
<div class="input-row">
  <input id="q" type="text" placeholder="shortcut…" autofocus />
  <button onclick="go()">Go</button>
</div>
<script>
  document.getElementById('q').focus();
  document.getElementById('q').addEventListener('keydown', function(e) {
    if (e.key === 'Enter') go();
    if (e.key === 'Escape') webkit.messageHandlers.gotoview.postMessage('');
  });
  function go() {
    webkit.messageHandlers.gotoview.postMessage(document.getElementById('q').value.trim());
  }
  // auto-size: report content height after render
  window.addEventListener('load', function() {
    var h = document.body.scrollHeight;
    webkit.messageHandlers.gotoview.postMessage('resize:' + h);
  });
</script>
</body>
</html>]]

    wv = hs.webview.new({x=x, y=y, w=w, h=h}, {developerExtrasEnabled=false}, usercontent)
    wv:windowStyle({})
    wv:allowTextEntry(true)
    wv:html(html)
    wv:show()
    wv:hswindow():focus()
end

return { show = show }
