---
name: firefox
description: "Control Firefox browser on macOS: open URLs, close/switch tabs, get active tab info, focus Firefox, send keystrokes. Use when asked to open a link in Firefox, close a tab, switch to a tab, find what's open in Firefox, or automate any browser action."
user_invocable: true
---

# Firefox Control

Automate Firefox via AppleScript + System Events UI scripting.

> All `osascript` calls require `dangerouslyDisableSandbox: true`.

## What works / doesn't

**Works natively:**
- Open a URL (new tab)
- Get active tab title per window
- Close a window
- Window properties (bounds, visibility, minimize)
- Activate/focus Firefox

**Works via UI scripting (System Events):**
- Close current tab (`Cmd+W`)
- Switch to a tab by name (`%<query>` in address bar)
- Get current tab URL (Cmd+L → select all → copy)
- Any keyboard shortcut / menu item

**Does NOT work:**
- Enumerate all tabs in a window (Firefox doesn't expose `tabs` as AppleScript objects)
- Get URLs without focusing the tab first

---

## Snippets

### Open a URL
```applescript
tell application "Firefox"
  activate
  open location "https://example.com"
end tell
```

### Get active tab titles (one per window)
```applescript
tell application "Firefox"
  set names to {}
  repeat with i from 1 to (count windows)
    try
      set end of names to (i as string) & ": " & name of window i
    end try
  end repeat
  return names
end tell
```

### Close a window by index
```applescript
tell application "Firefox"
  close window 2
end tell
```

### Get current tab URL
```applescript
tell application "Firefox"
  activate
end tell
delay 0.3
tell application "System Events"
  tell process "Firefox"
    keystroke "l" using command down
    delay 0.3
    keystroke "a" using command down
    delay 0.1
    keystroke "c" using command down
    delay 0.2
  end tell
end tell
do shell script "pbpaste"
```

### Switch to a tab by name / URL fragment
```applescript
-- Uses Firefox's built-in tab search: type "% <query>" in address bar
-- Must open a new tab first so the address bar is focused and ready
tell application "Firefox"
  activate
end tell
delay 0.5
tell application "System Events"
  tell process "Firefox"
    keystroke "t" using command down  -- open new tab (address bar auto-focused)
    delay 1
    keystroke "% YOUR_QUERY_HERE"
    delay 1.5
    key code 125  -- arrow down to first result
    delay 0.5
    key code 36   -- Enter to switch
  end tell
end tell
```

### Close a tab by URL fragment
```applescript
-- Open a new tab, search for the tab, switch to it, then close it
tell application "Firefox"
  activate
end tell
delay 0.5
tell application "System Events"
  tell process "Firefox"
    keystroke "t" using command down
    delay 1
    keystroke "% YOUR_QUERY_HERE"
    delay 1.5
    key code 125  -- arrow down to first result
    delay 0.5
    key code 36   -- Enter to switch to the found tab
    delay 0.8
    keystroke "w" using command down  -- close it
  end tell
end tell
```

### Close the current tab
```applescript
tell application "Firefox"
  activate
end tell
delay 0.2
tell application "System Events"
  tell process "Firefox"
    keystroke "w" using command down
  end tell
end tell
```

### Send any keyboard shortcut
```applescript
tell application "Firefox"
  activate
end tell
delay 0.2
tell application "System Events"
  tell process "Firefox"
    keystroke "r" using command down  -- e.g. Cmd+R to reload
  end tell
end tell
```

---

## Important: always confirm before closing

Before closing any tab or window, confirm with the user which one to close. Prefer identifying by title. Never assume which window index is which without first listing window names.

To identify windows before acting:
```applescript
tell application "Firefox"
  set names to {}
  repeat with i from 1 to (count windows)
    try
      set end of names to (i as string) & ": " & name of window i
    end try
  end repeat
  return names
end tell
```
