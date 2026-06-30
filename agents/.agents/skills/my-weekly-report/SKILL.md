---
name: my-weekly-report
description: >
  Generate a concise weekly status update in team format by pulling data from
  the current Jira sprint (tickets assigned to me), backlog, and recent activity.
  Outputs bullet points covering "What I worked on" (completed/in-progress this
  sprint), "What's next" (To Do in current sprint), and standard blockers/leaves
  fields. When asked for last week's report, covers completed items from last
  week. Upcoming items are based on the current sprint's To Do tickets.
  Triggers: /my-weekly-report, "my weekly update", "my weekly status", "generate my weekly",
  "write my weekly", "my weekly report"
user_invocable: true
---

# /my-weekly-report — Weekly Status Update Generator

Generate a concise weekly update in the team's standard format, based on the
current Jira sprint tickets assigned to you.

## Format

The output should follow the team format exactly:

```
## Abin Simon

**What I worked on:**
- <bullet for each Done/In Progress ticket, plus any notable non-Jira work from backlog>

**What's next:**
- <bullet for each To Do ticket in the sprint>

**Blockers:** None
**Upcoming leaves:** None
**Unplanned leaves last week:** None
```

Always output the report directly in the chat — do NOT write to a file.

## Steps

### 1. Find Active Sprint

```bash
jira sprint list --plain
```

Find the active sprint whose name contains "EARN" (that's the user's primary sprint).
Note its ID.

### 2. Get Sprint Tickets

```bash
~/.agents/skills/my-weekly-report/scripts/sprint-tickets.sh
```

Returns JSON with tickets grouped by status: `done`, `in_progress`, `to_do`.

### 3. Check Backlog for Notable Non-Jira Work

Read `/Users/meain/.local/share/sbdb/Backlog/Backlog.md` (first ~50 lines).

Look at **Today** and **This Week** sections for completed items (`- [x]`) that are
work-related and don't already have a corresponding Jira ticket. Include any notable
ones in "What I worked on".

### 4. Check GitHub PRs

```bash
~/.agents/skills/my-weekly-report/scripts/user-prs.sh "@me" $(cat ~/.config/datafiles/prs-repos)
```

Returns JSON array of PRs created or merged this week. Include merged PRs as
evidence of completed work. PRs still in review signal active "What I worked on"
items. Do NOT list PRs as separate bullets — use them to enrich Jira ticket bullets
or add a single "PR reviews" bullet if you reviewed several PRs by others.

### 5. Check Confluence for Recent Pages

```bash
~/.agents/skills/my-weekly-report/scripts/confluence-updates.sh
```

Returns text listing of pages modified this week. Parse the output to extract page titles.
If any design docs, runbooks, or notable pages were created or updated this week, add a
bullet for them in "What I worked on" (e.g. "Created design doc for Org Anchoring").

### 6. Build the Report

**What I worked on:**
- For Done tickets: one bullet per ticket, brief summary + Jira link
- For In Progress/IN REVIEW tickets: one bullet per ticket, brief summary + Jira link
- For any notable backlog completions: one bullet each

**What's next:**
- For To Do tickets: one bullet per ticket, brief summary + Jira link
- Keep bullets concise — match the style of prior updates in
  `/Users/meain/.local/share/sbdb/InfraCloud/Weekly Updates Submitted/`

**Blockers / leaves:**
- Default all three fields to "None" unless backlog or Jira indicates otherwise
- Check backlog for any explicit blocker notes or upcoming leave entries

### 7. Copy to Clipboard and Open Confluence

Print the draft in the chat, then copy it to the clipboard:

```bash
printf '%s' "<report text>" | pbcopy
```

Use `dangerouslyDisableSandbox: true`. Then find the Confluence page ID by searching
for "Updates for week of <current Monday's date>":

```bash
confluence search 'title = "Updates for week of <YYYY-MM-DD>" AND space = "~712020cccc16439d5041339f95122d8d977f63"' --cql 2>/dev/null
```

Open the page in the browser:

```bash
open "https://veeam-vdc.atlassian.net/wiki/spaces/~712020cccc16439d5041339f95122d8d977f63/pages/<pageId>/..."
```

The user will paste and save it manually in the browser.

Do NOT save the report locally — do not write to any file under
`/Users/meain/.local/share/sbdb/InfraCloud/Weekly Updates Submitted/`.

## Reference Format

See `/Users/meain/.local/share/sbdb/InfraCloud/Weekly Updates Submitted/2026/W17.md`
for the exact style. The Abin Simon section there is the canonical example:

```
## Abin Simon

**What I worked on:**
- Worked on separating earn query and ingestion service
- Working on pushing JSON schema validation to prod
- Security fixes in multiple repos
- Working with Rushikesh's (Veeam Employee) onboarding
- ADK SKU change and dropping DR for eventhub
- PR reviews
**What's next:**
- Org Anchoring
- Help with onboarding Rushikesh and work on synthetic monitoring

**Blockers:** None
**Upcoming leaves:** None
**Unplanned leaves last week:** April 14, 15
```

Note: bullets are short phrases, not full sentences. Jira links are optional but
preferred for tickets — include them when the ticket ID is known.
