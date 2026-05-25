---
name: backlog-review
description: >
  Update the daily backlog by gathering data from Jira sprint, GitHub PRs, and existing backlog,
  then collaboratively deciding what to work on today. Handles checked-off items, moves existing
  tasks to Today, and adds new items with reference links. Commits the backlog folder before
  making changes. Triggers: /backlog-review, /update-backlog, "update my backlog", "plan my backlog",
  "what should I work on today", "refresh backlog", "backlog update"
user_invocable: true
---

# /backlog-review -- Update Daily Backlog

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

## Step 0.5: Verify Today's Date

Always run `date "+%Y-%m-%d (%A)"` and treat its output as authoritative for the rest of the workflow. The system-injected `currentDate` in CLAUDE.md can lag by a day.

## Step 1: Read the Backlog

Read the full backlog file first to understand the current state before making any changes.

## Step 2: Handle Checked-Off Items in Today

Find all checked-off items (`- [x]`) in the Today section(s).

If there are checked-off items, use `AskUserQuestion` to ask what to do with them.
List all checked-off items in the question text and offer options:

1. **Move all to Before section (Recommended)** -- Append at the very end of the file (after the last existing dated section). The "Before" sections grow chronologically with the newest date at the bottom.
   Use the last working day's date. If today is Monday, use last Friday's date.
   If today is Sunday, use Friday. If today is Saturday, use Friday. Otherwise use yesterday.
   Format: `### YYYY-MM-DD (DayOfWeek)`
   IMPORTANT: Compute the date correctly. Verify the day-of-week name matches the computed date.
   Do NOT guess the day name. The default suggestion in the prompt should already use the correct
   last-working-day date (e.g., suggest Friday when running on Monday).
2. **Keep all in Today** -- Leave the items where they are

The user can also select "Other" to provide custom instructions (e.g., "move to Ongoing", "delete it").

After the user decides, apply the changes.

## Step 3: Gather Data from External Sources

Gather all data in parallel:

### 3a. Jira -- Current Sprint Tickets

```bash
jira issue list -q "sprint in openSprints() AND assignee = currentUser()" 2>&1
```

Note the ticket key, summary, priority, status, and story points.

### 3b. GitHub PRs -- Pending Review

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

### 3c. GitHub PRs -- Authored by User

Look up open PRs authored by the user across Veeam-VDC org. Surface PRs that:
- Have been approved (ready to merge)
- Have review comments to address
- Have requested changes

Add these to the summary under a "My PRs" section so the user can decide
which to act on today.

### 3d. Calendar (optional)

If MS365 MCP is available, check today's calendar for meetings that might affect capacity:
- Use `mcp__claude_ai_Microsoft_365__outlook_calendar_search` with today's date range
- This helps estimate available working hours

## Step 4: Present a Summary and Collaborate

Present a concise summary to the user:

### Already in Today
List items already in the Today section (unchecked).

### From Jira Sprint
List sprint tickets not yet represented in Today, grouped by priority.
For each, show: `[STATUS] TICKET-KEY: Summary (priority, story points)`

### My PRs (Approved / Needs Action)
List PRs authored by the user that are approved and ready to merge, or have review comments
to address. Each PR on its own line. These go at the top of the PR section since they're
actionable now.

### PRs Needing Review
List PRs awaiting review with repo, PR number, author, and age. Each PR on its own line.

### Candidates from Other Sections
Highlight items from Tomorrow/This Week/Whenever that might be worth pulling into Today.

### Capacity Estimate
Based on calendar meetings and estimated task sizes, suggest a reasonable workload.
A typical day has ~6 hours of productive work after meetings and routine tasks.

Present this summary as plain text with lettered/numbered references. Do NOT use `AskUserQuestion` for
this step — let the user respond conversationally. The user will reply with which items to include
(e.g., "B,C,D,K") or give other instructions. Iterate as needed until the user is satisfied.

### Interactive Selection via emacsclient (preferred)

For longer candidate lists, prefer this workflow over letter-selection:

1. Write candidate list to `/tmp/backlog-candidates-YYYY-MM-DD.md` (use today's date from Step 0.5 — e.g. `/tmp/backlog-candidates-2026-05-21.md`) to avoid collisions with stale files from prior sessions. Group by section, each candidate as a `- [ ]` line. Include a top-of-file instruction comment (lines starting with `#`) telling the user to delete lines they DON'T want and to save+close when done. Inline annotations starting with `>` or after the line are OK for free-form notes.
2. Open the file with `emacsclient /tmp/backlog-candidates-YYYY-MM-DD.md` -- this blocks until the user finishes editing and closes the buffer (`C-x #` or kill-buffer). Run with `dangerouslyDisableSandbox: true` since it talks to the user's emacs daemon. Use a long timeout (e.g. 600000ms). IMPORTANT: Write the file fresh BEFORE opening with emacsclient — do NOT call Write and emacsclient in parallel. Write may fail if the file already exists and hasn't been Read first, leaving emacsclient showing stale content.
3. When emacsclient returns, the file diff is surfaced via system-reminder. Re-read the file if needed. Treat any remaining `- [ ]` lines as the selected candidates. Honor free-form notes (e.g. `> all 3 as a single one` means combine those lines into one entry).
4. Apply edits to the backlog accordingly.

This avoids letter-mapping back-and-forth and lets the user freely annotate.

## Step 5: Update the Backlog

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
- Prefer inline links over trailing links. If the PR or ticket reference is already mentioned in the text, make it the link itself rather than duplicating it at the end.
  - Good: `Merge [control-plane-backend#123](https://github.com/...) (approved)`
  - Bad: `Merge control-plane-backend#123 (approved) [control-plane-backend#123](https://github.com/...)`
- Only use trailing `[ref](url)` links for URLs that don't have a natural inline anchor (e.g. Teams message links)
- Jira links: `[DP-1509](https://veeam-vdc.atlassian.net/browse/DP-1509)`
- PR links: `[control-plane-backend#123](https://github.com/Veeam-VDC/control-plane-backend/pull/123)`
- Generic refs: `[ref](url)`
- Priority emojis go after the description but before any trailing links
- Keep descriptions concise -- one line per task
- Each PR (to review or authored) must be on its own separate line -- never combine multiple PRs into one line
- Approved PRs needing merge or PRs with comments to address go near the top of the Today section, but after the standard morning routine items (charge devices, check emails/slack/teams/confluence)

## Notes

- Always show meeting times and timestamps in IST (Indian Standard Time, UTC+5:30), not UTC.
- The jira CLI and GH token extraction require running outside the sandbox (keychain access).
- Be reasonable with workload -- some leftover is fine, but don't overload the day.
- If the user has many meetings, suggest fewer coding tasks.
- Add morning routine items  at the top of Today before other tasks. These are standard items that should always be present:
  - Check emails
  - Check Slack
  - Check Teams
  - Check Confluence
  - Charge devices
- When moving items between sections, preserve their existing links and formatting exactly.
- Never remove generic section headers (Today, Tomorrow, This Week, Next Week, Whenever, Weekend, Ongoing). Leave them in place even when empty.
