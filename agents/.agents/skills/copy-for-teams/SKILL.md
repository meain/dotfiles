---
name: copy-for-teams
description: "Copy text to clipboard as rich HTML so it pastes with formatting into Microsoft Teams. Triggers: /copy-for-teams, 'copy for teams', 'teams message', 'copy to clipboard for teams'"
user_invocable: true
---

# Copy for Teams

When the user asks to copy something for Teams, format it as rich text (HTML) and copy to the macOS clipboard so it pastes with formatting via Cmd+V.

## How to copy

Write the HTML to a temp file, then use a separate python script with osascript to set the `public.html` pasteboard type. This requires `dangerouslyDisableSandbox: true` on the python command (clipboard access is blocked by sandbox).

Pipe the HTML content directly into the copy script (use Bash tool with `dangerouslyDisableSandbox: true`):

```bash
cat << 'HTMLEOF' | python3 ~/.claude/skills/copy-for-teams/copy_teams.py
<b>Title here</b><br>
<b>Key:</b> <code>value</code><br>
HTMLEOF
```

The copy script reads HTML from stdin and sets it on the clipboard as `public.html`.

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
