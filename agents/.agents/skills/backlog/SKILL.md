---
name: backlog-review
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

### Step 0: Commit Before Changes

```bash
cd /Users/meain/.local/share/sbdb && git add -A && git commit -m "Before backlog updates"
```
If no changes to commit, that's fine — continue.

### Step 0.5: Verify Today's Date

Always run `date "+%Y-%m-%d (%A)"` and treat its output as authoritative.

### Step 1: Read Backlog

Read the full backlog file first.

### Step 2: Handle Checked-Off Items

Find checked-off items (`- [x]`) in Today. Ask the user what to do:
1. **Move all to Before** (recommended) — Append at the very end with the last working day's date. Format: `### YYYY-MM-DD (DayOfWeek)`. Compute the date correctly.
2. **Keep all in Today** — Leave as-is.

### Step 3: Gather External Data (parallel)

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

**3d. Calendar (optional):** If MS365 MCP available, check today's meetings.

### Step 4: Present Summary

Present as plain text:
- **Already in Today** — unchecked items
- **From Jira Sprint** — tickets not yet in Today, grouped by priority
- **My PRs** — approved (ready to merge) and needs action
- **PRs Needing Review** — with repo, number, author
- **Candidates from other sections** — Tomorrow/This Week/Whenever items worth pulling in
- **Capacity estimate** — based on meetings and task sizes (~6h productive)

For longer lists, use the emacsclient workflow:
1. Write candidates to `/tmp/backlog-candidates-YYYY-MM-DD.md` with `- [ ]` lines
2. `emacsclient /tmp/backlog-candidates-YYYY-MM-DD.md` (blocking, 600s timeout)
3. Treat remaining `- [ ]` lines as selections

### Step 5: Update Backlog

Once confirmed:
1. Consolidate duplicate Today sections into one
2. Move items from other sections to Today (don't duplicate)
3. Add new items with reference links
4. Create task notes (`Tasks/YYYY-MM/`) for items needing significant context
5. Preserve priority emojis near top of Today

### Formatting Rules

- Prefer inline links over trailing links (e.g. `Merge [repo#123](url)`)
- Trailing `[ref](url)` only for URLs without a natural inline anchor (Teams links)
- Each PR must be on its own line
- Add morning routine items at top: Check emails, Check Slack, Check Teams, Check Confluence, Charge devices
- Never remove section headers (Today, Tomorrow, etc.) even when empty
- Show meeting times in IST (UTC+5:30)
