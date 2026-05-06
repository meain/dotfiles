---
name: daily-planner
description: >
  Generate an interactive daily planner HTML dashboard combining Jira sprint tickets,
  GitHub PRs pending review, vault backlog tasks, and Outlook calendar events.
  Outputs to ~/.local/share/daily-planner/<year>/<month>/<full-date>.html and opens it.
  Triggers: /daily-planner, "morning routine", "daily planner", "what's on my plate today",
  "morning dashboard", "plan my day", "generate daily planner"
---

# Daily Planner

Generate an interactive HTML dashboard for the day. The output file goes to
`~/.local/share/daily-planner/<YYYY>/<MM>/<YYYY-MM-DD>.html`.

## Data Sources

Gather all data in parallel where possible:

### 1. Jira — Sprint Tickets
```bash
# Outside sandbox (jira CLI needs keychain access)
jira issue list -q "sprint in openSprints() AND assignee = currentUser()" 2>&1
```
Group tickets by priority into categories:
- **Sprint — Highest**: priority = Highest
- **Sprint — High / In Progress**: priority = High OR status = "In Progress" OR status = "In Review"
- **Sprint — Medium**: everything else

Estimate time based on story points or past patterns. Default: Highest ~3h, High ~2h, Medium ~2h.

### 2. GitHub PRs — Pending Review
```bash
# Get GH token from keychain (outside sandbox)
GH_TOKEN=$(security find-generic-password -s "gh:github.com" -w 2>&1)
# Decode if base64-prefixed
if [[ "$GH_TOKEN" == go-keyring-base64:* ]]; then
  GH_TOKEN=$(echo "${GH_TOKEN#go-keyring-base64:}" | base64 -d)
fi
# Query via curl
curl -s -H "Authorization: bearer $GH_TOKEN" -H "Content-Type: application/json" \
  -d '{"query":"{ search(query: \"is:pr is:open review-requested:meain review:required org:Veeam-VDC\", type: ISSUE, first: 30) { nodes { ... on PullRequest { number title url repository { nameWithOwner } author { login } createdAt reviews(first: 10) { nodes { state author { login } } } reviewRequests(first: 20) { nodes { requestedReviewer { ... on User { login } ... on Team { name slug } } } } } } } }"}' \
  https://api.github.com/graphql
```

**Filter**:
- Only include PRs where `meain` appears as a direct User reviewer — exclude PRs
  where the only reviewers are teams (Datapipelines-codeowners, CP Platform, VDC Intelligence, etc.).
- Exclude PRs that are already approved (by anyone). Add `review:required` to the search
  query to only get PRs that still need approvals. Also include the `reviews` field in the
  GraphQL query and skip any PR where a review with state `APPROVED` exists.
  The search query should be: `is:pr is:open review-requested:meain review:required org:Veeam-VDC`

Mark PRs older than 90 days as `stale: true`.

Estimate: ~30m per PR, ~45m for large ones.

### 3. Vault Backlog
```bash
cat ~/.local/share/sbdb/Backlog/Backlog.md
```
Extract unchecked items (`- [ ]`) from the **first two "Today" sections** only.
Estimate ~15-30m for follow-ups, ~1h for collaboration tasks.

### 4. Calendar — Microsoft 365 MCP
Use the `mcp__claude_ai_Microsoft_365__outlook_calendar_search` tool:
- query: `*`
- afterDateTime: today 00:00
- beforeDateTime: tomorrow 00:00
- limit: 20

Convert UTC times to IST (+5:30). Deduplicate events with same subject and overlapping times.

### 5. Morning Routine (static)
Always include these items in a "Morning Routine" category:
- Check emails (~15m)
- Check Slack (~10m)
- Check Teams (~15m)
- Check Confluence (~10m)
- Cleanup backlog (~15m)

## HTML Generation

Use the template at `assets/template.html` as the reference for the HTML structure, CSS, and JavaScript.
The template includes all interactive features:
- Two-pane layout: checklist on left, details on right
- Checkboxes with localStorage persistence (key: `dp<MMDDYY>_chk`)
- Drag-and-drop reordering with localStorage persistence
- Collapsible categories with time totals
- Filter bar (hide done, hide stale)
- Keyboard navigation (j/k, x, Shift+J/K, c, 1-9)
- Confetti celebration on item check, bigger celebration on section complete
- Calendar strip (no checkboxes) between filter bar and main panes
- Progress bar and grand total remaining time in header

When generating the HTML:
1. Read the template from `assets/template.html`
2. Replace the `DATA` array with actual gathered data
3. Replace the calendar strip HTML with actual calendar events
4. Update the header date, sprint name, and sprint date range
5. Update localStorage keys to use the current date (`dp<MMDDYY>_chk`, `dp<MMDDYY>_ord`, `dp<MMDDYY>_col`)

## Output

1. Create directory: `mkdir -p ~/.local/share/daily-planner/<YYYY>/<MM>/`
2. Write HTML to: `~/.local/share/daily-planner/<YYYY>/<MM>/<YYYY-MM-DD>.html`
3. Open in browser: `open <path>` (requires dangerouslyDisableSandbox)

## Notes

- The jira CLI and GH token extraction require running outside the sandbox (keychain access).
- If MS365 MCP auth fails, skip the calendar section rather than falling back to icalBuddy.
- The sprint name comes from `jira sprint list --state active`.
- For day of week, compute from the date rather than hardcoding.
