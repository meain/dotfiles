---
name: backlog-add
description: >
  Look up a URL (Teams message, Jira ticket, GitHub PR/issue, incident.io alert) and create a
  concise backlog entry with a [ref] link. Triggers: /backlog-add, "add this link to backlog",
  "backlog from link", "look up and add to backlog"
---

# /backlog-add -- Create Backlog Entry from URL

Given a URL, fetch its content, extract a concise summary, and add it as a backlog task.

## Arguments

`/backlog-add <url> [section]`

- `url` (required): The URL to look up
- `section` (optional): Backlog section to add to (default: Today). Same keywords as /vault: today, tomorrow, this week, next week, whenever, ongoing, weekend, or a day name.

## Step 1: Classify the URL

Determine the source type from the URL pattern:

| Pattern | Type |
|---------|------|
| `teams.microsoft.com/l/message/` | Teams message |
| `veeam-vdc.atlassian.net/browse/` | Jira ticket |
| `github.com/.../pull/` | GitHub PR |
| `github.com/.../issues/` | GitHub issue |
| `app.incident.io/` | incident.io alert/incident |

## Step 2: Fetch Content

### Teams message
Use the Microsoft 365 MCP `read_resource` tool. Extract the `groupId` (called `teamId` in some URL formats) and `channelId` from the URL path, and the `messageId` from the path segment after `/messages/` or the timestamp.

URI format: `teams:///teams/{groupId}/channels/{channelId}/messages/{messageId}`

Extract from the response: sender name (`from.displayName`), message body text, any linked URLs within the message.

### Jira ticket
```bash
jira issue view PROJ-123 --plain
```

Extract: summary, status, assignee, type.

### GitHub PR
```bash
GIT_DIR=$(jj git root 2>/dev/null || echo .git) gh pr view <url> --json title,author,body,state
```

Extract: title, author, state.

### GitHub issue
```bash
GIT_DIR=$(jj git root 2>/dev/null || echo .git) gh issue view <url> --json title,author,body,state
```

Extract: title, author, state.

### incident.io
Use the incident.io MCP tools (`alert_show` or `incident_show`) to fetch the alert/incident details.

Extract: title, severity, status.

## Step 3: Compose Backlog Entry

Format: `- [ ] <concise description> [ref](<original-url>)`

Guidelines:
- Keep the description under ~80 visible chars (links render as their title text, so count the title not the URL)
- The description must be human-readable and meaningful on its own -- someone scanning the backlog should understand the task without clicking any link
- Include the person's first name if someone is asking/assigning something to the user
- For Jira: include ticket ID and summary (e.g., "Fix auth token storage bug [DP-1234](url)")
- For GitHub PRs: include repo#number and PR title/summary (e.g., "Review [control-plane-backend#4122](url): e2e tests use earn-query endpoints")
- For GitHub issues: include repo#number and issue title (e.g., "Fix [control-plane-backend#500](url): panic on nil config")
- For incidents/alerts: link to the incident (e.g., "Investigate [INC-456](url): RDIS 5xx surge in prod")
- For Teams: summarize the ask and use `[ref](url)` for the message link (e.g., "Review Org Anchoring CP spec from Robert [ref](url)")
- Do NOT duplicate links that are already discoverable from the ref URL -- the ref link is the entry point, keep it minimal
- Ask the user to confirm the entry text before adding

## Step 4: Add to Backlog

Use the `/vault` skill to add the composed entry to the specified section (default: Today).

If `/vault` is unavailable, fall back to directly editing `~/.local/share/sbdb/Backlog/Backlog.md`:
- Find the target section heading (e.g., `### Today`)
- Insert the new task at the END of that section (before the next `###` heading)
- Do not disturb other content
