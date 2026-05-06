---
name: update-backlog
description: >
  Update the daily backlog by gathering data from Jira sprint, GitHub PRs, and existing backlog,
  then collaboratively deciding what to work on today. Handles checked-off items, moves existing
  tasks to Today, and adds new items with reference links. Commits the backlog folder before
  making changes. Triggers: /update-backlog, "update my backlog", "plan my backlog",
  "what should I work on today", "refresh backlog", "backlog update"
user_invocable: true
---

# /update-backlog -- Update Daily Backlog

Collaboratively update the Today section of the backlog by pulling data from external sources
and working with the user to decide what's realistic for the day.

## Backlog Location

**File**: `/Users/meain/.local/share/sbdb/Backlog/Backlog.md`

The backlog has multiple sections: Today (may appear twice), Tomorrow, This Week, Weekend,
Next Week, Whenever, Ongoing, and dated "Before" sections at the bottom.

## Step 0: Commit Before Changes

Before making ANY modifications to the backlog, commit the current state:

```bash
cd /Users/meain/.local/share/sbdb && git add -A && git commit -m "Before backlog updates"
```

This creates a safety checkpoint. If the commit fails because there are no changes, that's fine -- continue.

## Step 1: Handle Checked-Off Items in Today

Read the backlog and find all checked-off items (`- [x]`) in the Today section(s).

If there are checked-off items, use `AskUserQuestion` to ask what to do with them.
List all checked-off items in the question text and offer options:

1. **Move all to Before section (Recommended)** -- Move to the bottom of the file under a new dated heading
   with yesterday's date. If today is Monday, use last Friday's date instead.
   Format: `### YYYY-MM-DD (DayOfWeek)`
2. **Keep all in Today** -- Leave the items where they are

The user can also select "Other" to provide custom instructions (e.g., "move to Ongoing", "delete it").

After the user decides, apply the changes.

## Step 2: Gather Data from External Sources

Gather all data in parallel:

### 2a. Jira -- Current Sprint Tickets

```bash
jira issue list -q "sprint in openSprints() AND assignee = currentUser()" 2>&1
```

Note the ticket key, summary, priority, status, and story points.

### 2b. GitHub PRs -- Pending Review

```bash
GH_TOKEN=$(security find-generic-password -s "gh:github.com" -w 2>&1)
if [[ "$GH_TOKEN" == go-keyring-base64:* ]]; then
  GH_TOKEN=$(echo "${GH_TOKEN#go-keyring-base64:}" | base64 -d)
fi
curl -s -H "Authorization: bearer $GH_TOKEN" -H "Content-Type: application/json" \
  -d '{"query":"{ search(query: \"is:pr is:open review-requested:meain review:required org:Veeam-VDC\", type: ISSUE, first: 30) { nodes { ... on PullRequest { number title url repository { nameWithOwner } author { login } createdAt reviews(first: 10) { nodes { state author { login } } } reviewRequests(first: 20) { nodes { requestedReviewer { ... on User { login } ... on Team { name slug } } } } } } } }"}' \
  https://api.github.com/graphql
```

Filter: Only PRs where `meain` is a direct User reviewer. Exclude PRs already approved.

### 2c. Current Backlog State

Read the full backlog file to understand:
- What's already in Today (unchecked items that remain)
- What's in Tomorrow, This Week, Next Week that might need to move up
- What's in Ongoing

### 2d. Calendar (optional)

If MS365 MCP is available, check today's calendar for meetings that might affect capacity:
- Use `mcp__claude_ai_Microsoft_365__outlook_calendar_search` with today's date range
- This helps estimate available working hours

## Step 3: Present a Summary and Collaborate

Present a concise summary to the user:

### Already in Today
List items already in the Today section (unchecked).

### From Jira Sprint
List sprint tickets not yet represented in Today, grouped by priority.
For each, show: `[STATUS] TICKET-KEY: Summary (priority, story points)`

### PRs Needing Review
List PRs awaiting review with repo, PR number, author, and age.

### Candidates from Other Sections
Highlight items from Tomorrow/This Week/Whenever that might be worth pulling into Today.

### Capacity Estimate
Based on calendar meetings and estimated task sizes, suggest a reasonable workload.
A typical day has ~6 hours of productive work after meetings and routine tasks.

Then use `AskUserQuestion` to ask the user which items to add to Today, whether to merge duplicates,
and any other decisions needed. Use numbered references from the summary so the user can respond concisely.
Iterate with follow-up questions if needed until the user is satisfied.

## Step 4: Update the Backlog

Once the user confirms the plan:

1. **Consolidate Today sections** -- If there are two Today sections, merge them into one.

2. **Move items to Today** -- For items coming from other backlog sections (Tomorrow, This Week, etc.),
   remove them from their current section and add to Today. Do not duplicate.

3. **Add new items** -- For Jira tickets or PR reviews not already in the backlog, add them to Today.

4. **Add reference links** at the end of each entry:
   - Jira tickets: `[TICKET-KEY](https://veeam-vdc.atlassian.net/browse/TICKET-KEY)`
   - GitHub PRs: `[repo#NUMBER](PR-URL)` e.g. `[control-plane-backend#3904](https://github.com/...)`
   - Teams/other refs: `[ref](url)`

5. **Task notes** -- If an item needs significant context (research findings, multiple steps, etc.),
   create a task note using the vault skill pattern:
   - File: `Tasks/YYYY-MM/<Task Name>.md`
   - Backlog entry: `- [ ] [[Tasks/YYYY-MM/Task Name]]`
   - Only do this when there's genuinely a lot of context to capture -- not for simple tasks.

6. **Preserve order** -- Keep high-priority items (with priority emojis) near the top of Today.

## Formatting Rules

- Use the exact task format from the vault skill (see vault SKILL.md for details)
- Links can be inline within the description, just not at the very start of the line
- Jira links: `[DP-1509](https://veeam-vdc.atlassian.net/browse/DP-1509)`
- PR links: `[control-plane-backend#123](https://github.com/Veeam-VDC/control-plane-backend/pull/123)`
- Generic refs: `[ref](url)`
- Priority emojis go after the description but before links
- Keep descriptions concise -- one line per task

## Notes

- The jira CLI and GH token extraction require running outside the sandbox (keychain access).
- Be reasonable with workload -- some leftover is fine, but don't overload the day.
- If the user has many meetings, suggest fewer coding tasks.
- Add morning routine items (Check emails, Check Slack, Check Teams, Check Confluence) at the top of Today before other tasks. These are standard items that should always be present.
- When moving items between sections, preserve their existing links and formatting exactly.
