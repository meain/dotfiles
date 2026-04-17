---
name: copy-for-teams
description: Copy text to clipboard as rich HTML so it pastes with formatting into Microsoft Teams. Triggers: /copy-for-teams, "copy for teams", "teams message", "copy to clipboard for teams"
user_invocable: true
---

# Copy for Teams

When the user asks to copy something for Teams, format it as rich text (HTML) and copy to the macOS clipboard so it pastes with formatting via Cmd+V.

## How to copy

Write the HTML to a temp file, then use a separate python script with osascript to set the `public.html` pasteboard type. This requires `dangerouslyDisableSandbox: true` on the python command (clipboard access is blocked by sandbox).

**Step 1**: Write the HTML content (use Bash tool, sandbox is fine here):

```bash
mkdir -p /tmp/claude && cat > /tmp/claude/teams_msg.html << 'HTMLEOF'
<b>Title here</b><br>
<b>Key:</b> <code>value</code><br>
HTMLEOF
```

**Step 2**: Write the copy script (use Write tool to `/tmp/claude/copy_teams.py`):

```python
import subprocess

html = open("/tmp/claude/teams_msg.html").read()
proc = subprocess.Popen(
    ["osascript", "-e", """
use framework "AppKit"
on run argv
    set htmlString to item 1 of argv
    set htmlData to (current application's NSString's stringWithString:htmlString)'s dataUsingEncoding:(current application's NSUTF8StringEncoding)
    set pb to current application's NSPasteboard's generalPasteboard()
    pb's clearContents()
    pb's setData:htmlData forType:"public.html"
end run
""", html], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
out, err = proc.communicate()
if proc.returncode != 0:
    print(err.decode())
else:
    print("OK")
```

**Step 3**: Run the copy script (use Bash tool with `dangerouslyDisableSandbox: true`):

```bash
python3 /tmp/claude/copy_teams.py
```

The python script only needs to be written once per session. For subsequent copies, just update the HTML file and re-run the python script.

## Formatting guidelines

Use these HTML tags for formatting — Teams renders them when pasted via Cmd+V:

- `<b>` — bold text
- `<code>` — inline code
- `<br>` — line break
- `<hr>` — horizontal rule
- `<blockquote>` — quoted text
- `<ul>`, `<li>` — bullet lists
- `<ol>`, `<li>` — numbered lists

Keep the HTML simple and flat. Do not use CSS styles or complex nesting — Teams strips most of it.

## Tone

- Use proper capitalization (start sentences with capitals).
- No greeting or sign-off — just the message content directly.
- Keep it casual and conversational, but not sloppy.
