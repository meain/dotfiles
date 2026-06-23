---
name: backlog
description: >
  Daily backlog management — full planning review OR add a single entry from a URL.
  Full review: gathers Jira sprint, GitHub PRs, checked-off items, and collaboratively
  plans the day. Add from URL: looks up a Teams message, Jira ticket, GitHub PR/issue,
  or incident.io alert and creates a concise backlog entry with a [ref] link.
  Triggers: /backlog-review, /update-backlog, "update my backlog", "plan my backlog",
  "what should I work on today", "refresh backlog", "backlog update",
  /backlog-add, "add this link to backlog", "backlog from link", "look up and add to backlog"
user_invocable: true
---

# /backlog — Daily Backlog Management

Two modes: **full daily review** (default) or **add single entry from URL** (when a URL is provided).

## Backlog Location

**File**: `/Users/meain/.local/share/sbdb/Backlog/Backlog.md`
Sections: Today, Tomorrow, This Week, Weekend, Next Week, Whenever, Ongoing, and dated "Before" sections.

---

## Mode A: Add Entry from URL

When given a URL, classify the source, fetch content, and add a task to the backlog.

### Step 1: Classify

| Pattern | Type |
|---------|------|
| `teams.microsoft.com/l/message/` | Teams message |
| `veeam-vdc.atlassian.net/browse/` | Jira ticket |
| `github.com/.../pull/` | GitHub PR |
| `github.com/.../issues/` | GitHub issue |
| `app.incident.io/` | incident.io alert/incident |

### Step 2: Fetch Content

- **Teams**: Use MS365 MCP `read_resource` with URI `teams:///teams/{groupId}/channels/{channelId}/messages/{messageId}`. Extract sender name, message body.
- **Jira**: `jira issue view PROJ-123 --plain` — extract summary, status, assignee, type.
- **GitHub PR**: `gh pr view <url> --json title,author,body,state`
- **GitHub issue**: `gh issue view <url> --json title,author,body,state`
- **incident.io**: Use incident.io MCP tools (`alert_show` or `incident_show`).

### Step 3: Compose Entry

Format: `- [ ] <concise description> [ref](<original-url>)`

Guidelines:
- Keep description under ~80 visible chars
- Include person's first name if someone is asking/assigning something
- Jira: `Fix auth token storage bug [DP-1234](url)`
- GitHub PR: `Review [repo#number](url): summary`
- Teams: `Review topic from Name [ref](url)`

### Step 4: Add to Backlog

Use `/vault` skill to add to the specified section (default: Today). If unavailable, directly edit `Backlog.md` — find the target section heading and insert at the end of that section.

---

## Mode B: Full Daily Review

Collaboratively update the Today section of the backlog by pulling data from external sources and working with the user to decide what's realistic.

### Step 0: Verify Today's Date

Always run `date "+%Y-%m-%d (%A)"` and treat its output as authoritative.

### Step 1: Read Backlog

Read the full backlog file first.

### Step 2: Handle Checked-Off Items

Note the checked-off items from Today — they will be archived to Before during Step 5. Format: `### YYYY-MM-DD (DayOfWeek)`. Never reason about the date mentally — compute it with:
```bash
YESTERDAY=$(date -d "yesterday" "+%A"); case "$YESTERDAY" in Saturday) date -d "2 days ago" "+%Y-%m-%d (%A)";; Sunday) date -d "3 days ago" "+%Y-%m-%d (%A)";; *) date -d "yesterday" "+%Y-%m-%d (%A)";; esac
```

Insert the new date block at the **end (bottom)** of the Before section — Before is chronologically ascending (oldest at top, newest at bottom).

### Step 3: Gather External Data (parallel)

> **All Jira and GitHub commands must be run with `dangerouslyDisableSandbox: true`** — the sandbox blocks TLS certificate verification needed for these hosts.

**3a. Jira sprint:**
```bash
jira issue list -q "sprint in openSprints() AND assignee = currentUser()" 2>&1
```

**3b. GitHub PRs — pending reviews:**
```bash
GH_TOKEN=$(security find-generic-password -s "gh:github.com" -w 2>&1)
if [[ "$GH_TOKEN" == go-keyring-base64:* ]]; then
  GH_TOKEN=$(echo "${GH_TOKEN#go-keyring-base64:}" | base64 -d)
fi
curl -s -H "Authorization: bearer $GH_TOKEN" -H "Content-Type: application/json" \
  -d '{"query":"{ search(query: \"is:pr is:open review-requested:meain review:required org:Veeam-VDC\", type: ISSUE, first: 30) { nodes { ... on PullRequest { number title url isDraft repository { nameWithOwner } author { login } createdAt reviews(first: 10) { nodes { state author { login } } } reviewRequests(first: 20) { nodes { requestedReviewer { ... on User { login } ... on Team { name slug } } } } } } } }"}' \
  https://api.github.com/graphql
```
Filter: only direct reviews (not team), exclude approved PRs and drafts.

**3c. PRs authored by user:** Open PRs authored by the user across Veeam-VDC org.

**3d. Calendar:** Use MS365 MCP `outlook_calendar_search` to fetch today's meetings.

### Step 4: Select Candidates via Emacs

Write all candidates to `/tmp/backlog-candidates-YYYY-MM-DD.md` with `- [ ]` lines, grouped by:
- Morning routine (Apply rosemary water, Apply sunscreen, Check emails, Check Slack, Check Teams, Charge devices)
- Today's meetings (from calendar — list as info, no checkboxes; don't add to backlog)
- My PRs — approved (ready to merge)
- My PRs — needs action
- PRs needing direct review (only where `login: meain` is a direct reviewer, not team; exclude PRs authored by `rami-veeam` or `tford-veeam`; exclude PRs with "local-dev" in the title)
- Candidates from other sections (Tomorrow/This Week/Whenever worth pulling in)

The "Checked-off items to move to Before" header must include the day name, e.g. `(2026-06-03, Wednesday)`.

Invoke the `/edit-in-emacs` skill with the file path (blocking, 600s timeout). After it returns, treat remaining `- [ ]` lines as selections.

**Never use AskUserQuestion / multi-select prompts** — always go straight to the emacs file.

### Step 5: Commit Then Update Backlog

Before writing any changes to the backlog file, commit the current state:

```bash
cd /Users/meain/.local/share/sbdb && git add -A && git commit -m "Before backlog updates"
```

Then:

1. Move items from other sections to Today (don't duplicate)
2. Add new items with reference links
3. Preserve priority emojis near top of Today

### Formatting Rules

- Prefer inline links over trailing links (e.g. `Merge [repo#123](url)`)
- Trailing `[ref](url)` only for URLs without a natural inline anchor (Teams links)
- Each PR must be on its own line
- Never remove section headers (Today, Tomorrow, etc.) even when empty
- Show meeting times in IST (UTC+5:30)
