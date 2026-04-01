---
name: copy-for-teams
description: Copy text to clipboard as rich HTML so it pastes with formatting into Microsoft Teams. Triggers: /copy-for-teams, "copy for teams", "teams message", "copy to clipboard for teams"
user_invocable: true
---

# Copy for Teams

When the user asks to copy something for Teams, format it as rich text (HTML) and copy to the macOS clipboard so it pastes with formatting via Cmd+V.

## How to copy

Write the HTML to a temp file and use python3+osascript to set the `public.html` pasteboard type:

```bash
cat <<'EOF' > /tmp/teams_msg.html
<b>Title here</b><br>
<b>Key:</b> <code>value</code><br>
<!-- ... rest of HTML ... -->
EOF

python3 -c "
import subprocess
html = open('/tmp/teams_msg.html').read()
proc = subprocess.Popen(
    ['osascript', '-e', '''
use framework \"AppKit\"
on run argv
    set htmlString to item 1 of argv
    set htmlData to (current application's NSString's stringWithString:htmlString)'s dataUsingEncoding:(current application's NSUTF8StringEncoding)
    set pb to current application's NSPasteboard's generalPasteboard()
    pb's clearContents()
    pb's setData:htmlData forType:\"public.html\"
end run
''', html], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
out, err = proc.communicate()
if proc.returncode != 0:
    print(err.decode())
else:
    print('OK')
"
```

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
